{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Coerce
import Data.List (isInfixOf)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified DataCon as GHC
import qualified DynFlags as GHC
import FastString (unpackFS)
import qualified GHC
import GHC.PackageDb
import qualified GHC.PackageDb as GHC
import HscTypes (ModIface(..))
import HsInspect.Json ()
import HsInspect.Sexp
import HsInspect.Util
import qualified Id as GHC
import Module (Module(..), moduleNameString)
import Module as GHC
import qualified Name as GHC
import Outputable (showPpr)
import qualified Outputable as GHC
import PackageConfig
import qualified PackageConfig as GHC
import Packages (explicitPackages, getPackageDetails, lookupPackage)
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
      spid = sourcePackageId $ getPackageDetails dflags unitid
      dirs = maybeToList $ GHC.hiDir dflags
  home_mods <- getTargetModules
  home_entries <- getSymbols spid True [] home_mods dirs

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
  (_, res) <- liftIO . initTcInteractive env $ do
    iface <- readBinIface IgnoreHiWay QuietBinIFaceReading hi
    f iface
  pure res

getPkgSymbols :: GHC.GhcMonad m => PackageConfig -> m PackageEntries
getPkgSymbols pkg = do
  dflags <- GHC.getSessionDynFlags
  let unitid = GHC.packageConfigId pkg
      inplace = "-inplace" `isInfixOf` (GHC.unitIdString unitid)
      spid = sourcePackageId $ getPackageDetails dflags unitid
      exposed = Set.fromList $ fst <$> exposedModules pkg
      dirs = (importDirs pkg)
      haddocks = GHC.haddockHTMLs pkg
  if Set.null exposed || null dirs
    then pure $ PackageEntries spid inplace [] haddocks
    else getSymbols spid inplace haddocks exposed dirs

getSymbols :: GHC.GhcMonad m => SourcePackageId -> Bool -> [FilePath] -> Set GHC.ModuleName -> [FilePath] -> m PackageEntries
getSymbols spid inplace haddocks exposed dirs = do
  let findHis dir = liftIO $ walkSuffix ".hi" dir
  his <- join <$> traverse findHis dirs
  dflags <- GHC.getSessionDynFlags
  symbols <- catMaybes <$> traverse (hiToSymbols exposed) his
  let entries = uncurry mkEntries <$> symbols
      mkEntries m things = ModuleEntries (moduleName m) (renderThings things)
      renderThings things = catMaybes $ (uncurry $ tyrender dflags) <$> things
  pure $ PackageEntries spid inplace entries haddocks

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
tyrender dflags m' (GHC.AGlobal thing) =
  let
    m = mkMod dflags <$> m'
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

-- Bool indicates if this is an -inplace package
data PackageEntries = PackageEntries SourcePackageId Bool [ModuleEntries] Haddocks

data Mod = Mod SourcePackageId GHC.ModuleName

mkMod :: GHC.DynFlags -> Module -> Mod
mkMod dflags m = Mod
  (sourcePackageId $ getPackageDetails dflags (moduleUnitId m))
  (moduleName m)

-- TODO don't include srcid if it matches the current module
instance ToSexp Mod where
  toSexp (Mod spid name) = alist
    [ ("srcid", SexpString . unpackFS . coerce $ spid),
      ("module", SexpString . moduleNameString $ name) ]

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
  toSexp (PackageEntries spid inplace modules haddocks) =
    alist
      [ ("srcid", toSexp . unpackFS . coerce $ spid),
        ("inplace", toSexp inplace),
        ("modules", toSexp modules),
        ("haddocks", toSexp haddocks) ]

