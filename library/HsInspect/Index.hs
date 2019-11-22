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
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import GHC.PackageDb
import HscTypes (ModIface(..))
import HsInspect.Json ()
import HsInspect.Sexp
import HsInspect.Util
import qualified Id as GHC
import Module (Module(..), moduleNameString, unitIdString)
import qualified Name as GHC
import Outputable (showPpr)
import qualified Outputable as GHC
import PackageConfig
import PackageConfig (packageConfigId)
import Packages (explicitPackages, lookupPackage)
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

  -- TODO similarly to the Imports case, this can be slow if ghc thinks anything
  -- is out of date and tries to compile it. However, here we can choose to
  -- ignore anything that hasn't been compiled. We could filter the targets list
  -- based on the .hi files that we see in the output directory.
  _ <- GHC.load $ GHC.LoadAllTargets
  let unitid = GHC.thisPackage dflags
      dirs = maybeToList $ GHC.hiDir dflags
  home_mods <- getHomeModules
  home_entries <- getSymbols unitid home_mods dirs

  pure $ home_entries : deps

getPkgSymbols :: GHC.GhcMonad m => PackageConfig -> m PackageEntries
getPkgSymbols pkg =
  let unitid = packageConfigId pkg
      exposed = Set.fromList $ fst <$> exposedModules pkg
      dirs = (importDirs pkg)
   in if Set.null exposed || null dirs
        then pure $ PackageEntries unitid []
        else getSymbols unitid exposed dirs

getSymbols :: GHC.GhcMonad m => GHC.UnitId -> Set GHC.ModuleName -> [FilePath] -> m PackageEntries
getSymbols unitid exposed dirs = do
  let findHis dir = filter (".hi" `isSuffixOf`) <$> liftIO (walk dir)
  his <- join <$> traverse findHis dirs
  dflags <- GHC.getSessionDynFlags
  symbols <- catMaybes <$> traverse (hiToSymbols exposed) his
  let entries = uncurry mkEntries <$> symbols
      mkEntries m things = ModuleEntries (moduleName m) (renderThings things)
      renderThings things = catMaybes $ (uncurry $ tyrender dflags) <$> things
  pure $ PackageEntries unitid entries

hiToSymbols
  :: GHC.GhcMonad m
  => Set GHC.ModuleName
  -> FilePath
  -> m (Maybe (GHC.Module, [(Maybe GHC.Module, GHC.TcTyThing)]))
hiToSymbols exposed hi = do
  env <- GHC.getSession
  (_, hits) <-
    -- TODO use initTc instead of initTcInteractive
    liftIO . initTcInteractive env $ do
      iface <- readBinIface IgnoreHiWay QuietBinIFaceReading hi
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
  pure $ join hits

tyrender :: GHC.DynFlags -> Maybe GHC.Module -> GHC.TcTyThing -> Maybe Entry
tyrender dflags ((Mod <$>) -> m) (GHC.AGlobal thing) =
  let
    shw :: GHC.Outputable m => m -> String
    shw = showPpr dflags
   in case thing of
    (GHC.AnId var) -> Just $ IdEntry m
      (shw $ GHC.idName var)
      (shw $ GHC.idType var) -- TODO fully qualify?
    (GHC.AConLike (GHC.RealDataCon dc)) -> Just $ ConEntry m
      (shw $ GHC.getName dc)
      (shw $ GHC.dataConUserType dc) -- TODO fully qualify?
    -- TODO PatSynCon
    (GHC.ATyCon tc) -> Just $ TyConEntry m
      (shw $ GHC.tyConName tc)
      (shw $ GHC.tyConFlavour tc)
    _ -> Nothing
tyrender _ _ _ = Nothing

-- TODO normalise the type string to make it easier for downstream tools to perform searches
data Entry = IdEntry (Maybe Mod) String String -- ^ name type
           | ConEntry (Maybe Mod) String String -- ^ name type
           | TyConEntry (Maybe Mod) String String -- ^ type flavour

data ModuleEntries = ModuleEntries GHC.ModuleName [Entry]

-- FIXME the packagedb file, so editors can use heuristics to look for source code
-- TODO Maybe haddock-html
data PackageEntries = PackageEntries GHC.UnitId [ModuleEntries]

newtype Mod = Mod GHC.Module

instance ToSexp Mod where
  toSexp (Mod m) = alist
    [ ("unitid", SexpString . unitIdString . moduleUnitId $ m),
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
  toSexp (PackageEntries pkg modules) =
    alist
      [ ("unitid", SexpString . unitIdString $ pkg),
        ("modules", toSexp modules)
      ]
