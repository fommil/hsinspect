{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dumps an index of all terms and their types
module HsInspect.Index
  ( index,
    PackageEntries,
  )
where

import Avail (AvailInfo(..))
import BinIface (CheckHiWay(..), TraceBinIFaceReading(..), readBinIface)
import qualified ConLike as GHC
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import GHC.PackageDb
import qualified GHC.PackageDb as GHC
import HscTypes (ModIface(..))
import HsInspect.Json ()
import HsInspect.Sexp
import HsInspect.Util
import qualified Id as GHC
import Module (Module(..), moduleNameString)
import qualified Name as GHC
import Outputable (showPpr)
import qualified Outputable as GHC
import PackageConfig
import qualified PackageConfig as GHC
import Packages (explicitPackages, lookupPackage)
--import System.IO (hPutStrLn, stderr)
import TcEnv (tcLookup)
import TcRnMonad (initTcInteractive)
import qualified TcRnTypes as GHC
import qualified TyCon as GHC

index :: GHC.GhcMonad m => m [PackageEntries]
index = do
  dflags <- GHC.getSessionDynFlags

  let explicit = explicitPackages $ GHC.pkgState dflags
      pkgcfgs = maybeToList . lookupPackage dflags =<< explicit
  deps <- traverse getPkgSymbols pkgcfgs

  loadCompiledModules
  let unitid = GHC.thisPackage dflags
      dirs = maybeToList $ GHC.hiDir dflags
  home_mods <- getTargetModules
  home_entries <- getSymbols unitid [] home_mods dirs

  pure $ home_entries : deps

-- finds the module names of all the .hi files in the output directory and then
-- tells ghc to load them as the only targets. Compared to loading all the home
-- modules provided by the ghcflags, this means that ghc can only see the
-- contents of compiled files and will not attempt to compile any source code.
-- Obviously comes with caveats but will be much faster if the preferred
-- behaviour is to fail fast with partial data instead of trying (futilely) to
-- compile all home modules with the interactive compiler.
loadCompiledModules :: GHC.GhcMonad m => m ()
loadCompiledModules = do
  dflags <- GHC.getSessionDynFlags
  case GHC.hiDir dflags of
    Nothing -> pure ()
    Just dir -> do
      compiled <- getCompiledTargets dir
      GHC.setTargets compiled
      void . GHC.load $ GHC.LoadAllTargets

getCompiledTargets :: GHC.GhcMonad m => FilePath -> m [GHC.Target]
getCompiledTargets dir = do
  provided <- getTargetModules
  his <- liftIO $ walkSuffix ".hi" dir
  modules <- catMaybes <$> traverse (flip withHi (pure . mi_module)) his
  let toTarget m =
        if Set.member m provided
          then Just $ GHC.Target (GHC.TargetModule m) True Nothing
          else Nothing
  pure $ mapMaybe (toTarget . moduleName) modules

-- Perform an operation given the parsed .hi file. tcLookup will only succeed if
-- the module is on the packagedb or is a home module that has been loaded.
withHi :: GHC.GhcMonad m => FilePath -> (GHC.ModIface -> (GHC.TcRnIf GHC.TcGblEnv GHC.TcLclEnv) a) -> m (Maybe a)
withHi hi f = do
  env <- GHC.getSession
  -- TODO use initTc instead of initTcInteractive
  (_, res) <- liftIO . initTcInteractive env $ do
    iface <- readBinIface IgnoreHiWay QuietBinIFaceReading hi
    f iface
  pure res

getPkgSymbols :: GHC.GhcMonad m => PackageConfig -> m PackageEntries
getPkgSymbols pkg =
  let unitid = GHC.packageConfigId pkg
      exposed = Set.fromList $ fst <$> exposedModules pkg
      dirs = (importDirs pkg)
      haddocks = GHC.haddockHTMLs pkg
   in if Set.null exposed || null dirs
        then pure $ PackageEntries unitid [] haddocks
        else getSymbols unitid haddocks exposed dirs

getSymbols :: GHC.GhcMonad m => GHC.UnitId -> [FilePath] -> Set GHC.ModuleName -> [FilePath] -> m PackageEntries
getSymbols unitid haddocks exposed dirs = do
  let findHis dir = liftIO $ walkSuffix ".hi" dir
  his <- join <$> traverse findHis dirs
  dflags <- GHC.getSessionDynFlags
  symbols <- catMaybes <$> traverse (hiToSymbols exposed) his
  let entries = uncurry mkEntries <$> symbols
      mkEntries m things = ModuleEntries (moduleName m) (renderThings things)
      renderThings things = catMaybes $ (uncurry $ tyrender dflags) <$> things
  pure $ PackageEntries unitid entries haddocks

-- for a .hi file returns the module and a list of all things (with types
-- resolved) in that module and their original module if they are re-exported.
hiToSymbols
  :: GHC.GhcMonad m
  => Set GHC.ModuleName
  -> FilePath
  -> m (Maybe (GHC.Module, [(Maybe GHC.Module, GHC.TcTyThing)]))
hiToSymbols exposed hi = (join <$>) <$> withHi hi $ \iface -> do
  let m = mi_module iface
  if not $ Set.member (GHC.moduleName m) exposed
    then pure Nothing
    else do
      let thing (Avail name) = traverse tcLookup' [name]
          -- TODO the fields in AvailTC
          thing (AvailTC _ members _) = traverse tcLookup' members
          reexport name = do
            modl <- GHC.nameModule_maybe name
            if m == modl then Nothing else Just modl
          tcLookup' name = (reexport name,) <$> tcLookup name
      things <- join <$> traverse thing (mi_exports iface)
      pure . Just $ (m, things)

tyrender :: GHC.DynFlags -> Maybe GHC.Module -> GHC.TcTyThing -> Maybe Entry
tyrender dflags ((Mod <$>) -> m) (GHC.AGlobal thing) =
  let
    shw :: GHC.Outputable m => m -> String
    shw = showPpr dflags
   in case thing of
    (GHC.AnId var) -> Just $ IdEntry m
      (shw $ GHC.idName var)
      (shw $ GHC.idType var) -- TODO fully qualify
    (GHC.AConLike (GHC.RealDataCon dc)) -> Just $ ConEntry m
      (shw $ GHC.getName dc)
      (shw $ GHC.dataConUserType dc) -- TODO fully qualify
    -- TODO PatSynCon
    (GHC.ATyCon tc) -> Just $ TyConEntry m
      (shw $ GHC.tyConName tc)
      (shw $ GHC.tyConFlavour tc)
    _ -> Nothing
tyrender _ _ _ = Nothing

data Entry = IdEntry (Maybe Mod) String String -- ^ name type
           | ConEntry (Maybe Mod) String String -- ^ name type
           | TyConEntry (Maybe Mod) String String -- ^ type flavour

data ModuleEntries = ModuleEntries GHC.ModuleName [Entry]

-- The haddocks serve a dual purpose: not only do they point to where haddocks
-- might be, they give a hint to the text editor where the sources for this
-- package are (e.g. with the ghc distribution, build tool store or local).
--
-- Users should type `cabal haddock --enable-documentation` to populate the docs
-- of their dependencies and local projects.
type Haddocks = [FilePath]

data PackageEntries = PackageEntries GHC.UnitId [ModuleEntries] Haddocks

newtype Mod = Mod GHC.Module

instance ToSexp Mod where
  toSexp (Mod m) = alist
    [ ("unitid", SexpString . normaliseUnitId . moduleUnitId $ m),
      ("module", SexpString . moduleNameString . moduleName $ m) ]

instance ToSexp Entry where
  toSexp (IdEntry m name typ) = alist
    [ ("name", SexpString name),
      ("type", SexpString typ),
      ("class", "id"),
      ("export", toSexp m)]
  toSexp (ConEntry m name typ) = alist
    [ ("name", SexpString name),
      ("type", SexpString typ),
      ("class", "con"),
      ("export", toSexp m) ]
  toSexp (TyConEntry m typ flavour) = alist
    [ ("type", SexpString typ),
      ("class", "tycon"),
      ("flavour", SexpString flavour),
      ("export", toSexp m) ]

instance ToSexp ModuleEntries where
  toSexp (ModuleEntries modl entries) =
    alist
      [ ("module", SexpString . moduleNameString $ modl),
        ("ids", toSexp entries)
      ]

instance ToSexp PackageEntries where
  toSexp (PackageEntries unitid modules haddocks) =
    alist
      [ ("unitid", SexpString . normaliseUnitId $ unitid),
        ("modules", toSexp modules),
        ("haddocks", toSexp haddocks) ]
