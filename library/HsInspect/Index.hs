{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Dumps an index of all terms and their types
module HsInspect.Index
  ( index,
    PackageEntries,
  )
where

#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
import qualified GHC.Driver.Session as GHC
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
import qualified GHC.Data.ShortText as GHC
import qualified GHC.Driver.Env.Types as GHC
import qualified GHC.Driver.Ppr as GHC
import qualified GHC.Unit.Env as GHC
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Core.ConLike as GHC
import qualified GHC.Core.PatSyn as GHC
import qualified GHC.Core.TyCon as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Binary as GHC
import qualified GHC.Tc.Types as GHC
import qualified GHC.Tc.Utils.Env as GHC
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.Id as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Unit.Database as GHC
import qualified GHC.Unit.State as GHC
import qualified GHC.Unit.Types as GHC
import qualified GHC.Utils.Outputable as GHC
#else
import qualified Avail as GHC
import qualified BinIface as GHC
import qualified ConLike as GHC
import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified GHC.PackageDb as GHC
import qualified Id as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified PackageConfig as GHC
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified TcEnv as GHC
import qualified TcRnMonad as GHC
import qualified TyCon as GHC
#endif
import qualified GHC

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf, sort)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import HsInspect.Sexp
import HsInspect.Util

-- TODO export unexposed modules too, since they could be exposed by an export elsewhere
--
-- TODO modules that export other modules seem to be skipped, e.g. Language.Haskell.LSP.Types
index :: GHC.GhcMonad m => m [PackageEntries]
index = do
  dflags <- GHC.getSessionDynFlags

#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
  let unarg as = fst <$> as
#elif MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  let unarg = id
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  sess <- GHC.getSession
  let unit_state = GHC.ue_units $ GHC.hsc_unit_env sess
      explicit = GHC.explicitUnits unit_state
      pkgcfgs = maybeToList . GHC.lookupUnit unit_state =<< unarg explicit
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  let unit_state = GHC.unitState dflags
      explicit = GHC.explicitUnits unit_state
      pkgcfgs = maybeToList . GHC.lookupUnit unit_state =<< explicit
#else
  let explicit = GHC.explicitPackages $ GHC.pkgState dflags
      pkgcfgs = maybeToList . GHC.lookupPackage dflags =<< explicit
#endif
  deps <- traverse getPkgSymbols pkgcfgs

  loadCompiledModules
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  let unitid = GHC.homeUnitId_ dflags
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  let unitid = GHC.homeUnitId dflags
#else
  let unitid = GHC.thisPackage dflags
#endif
      dirs = maybeToList $ GHC.hiDir dflags
  home_mods <- getTargetModules
  home_entries <- getSymbols unitid True [] home_mods dirs

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
  modules <- catMaybes <$> traverse (flip withHi (pure . GHC.mi_module)) his
#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
  sess <- GHC.getSession
  let unitid = GHC.ue_current_unit $ GHC.hsc_unit_env sess
      mkTarget m = GHC.Target (GHC.TargetModule m) True unitid Nothing
#else
  let mkTarget m = GHC.Target (GHC.TargetModule m) True Nothing
#endif
      toTarget m =
        if Set.member m provided
          then Just $ mkTarget m
          else Nothing
  pure $ mapMaybe (toTarget . GHC.moduleName) modules

-- Perform an operation given the parsed .hi file. tcLookup will only succeed if
-- the module is on the packagedb or is a home module that has been loaded.
withHi :: GHC.GhcMonad m => FilePath -> (GHC.ModIface -> (GHC.TcRnIf GHC.TcGblEnv GHC.TcLclEnv) a) -> m (Maybe a)
withHi hi f = do
  env <- GHC.getSession
#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
  dflags <- GHC.getSessionDynFlags
  let profile = GHC.targetProfile dflags
      name_cache = GHC.hsc_NC env
  (_, res) <- liftIO $ do
        iface <- GHC.readBinIface profile name_cache GHC.IgnoreHiWay GHC.QuietBinIFace hi
        GHC.initTcInteractive env $ f iface
#elif MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  (_, res) <- liftIO . GHC.initTcInteractive env $ do
        iface <- GHC.readBinIface GHC.IgnoreHiWay GHC.QuietBinIFace hi
        f iface
#else
  (_, res) <- liftIO . GHC.initTcInteractive env $ do
        iface <- GHC.readBinIface GHC.IgnoreHiWay GHC.QuietBinIFaceReading hi
        f iface
#endif
  pure res

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
getPkgSymbols :: GHC.GhcMonad m => GHC.UnitInfo -> m PackageEntries
#else
getPkgSymbols :: GHC.GhcMonad m => GHC.PackageConfig -> m PackageEntries
#endif
getPkgSymbols pkg =
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  let exposed = Set.fromList $ fst <$> GHC.unitExposedModules pkg
      GHC.GenericUnitInfo {GHC.unitId = unitid} = pkg
      dirs = GHC.unpack <$> (GHC.unitImportDirs pkg)
      haddocks = GHC.unpack <$> GHC.unitHaddockHTMLs pkg
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  let exposed = Set.fromList $ fst <$> GHC.unitExposedModules pkg
      GHC.GenericUnitInfo {GHC.unitId = unitid} = pkg
      dirs = GHC.unitImportDirs pkg
      haddocks = GHC.unitHaddockHTMLs pkg
#else
  let exposed = Set.fromList $ fst <$> GHC.exposedModules pkg
      unitid = GHC.packageConfigId pkg
      dirs = (GHC.importDirs pkg)
      haddocks = GHC.haddockHTMLs pkg
#endif
      unit_string = GHC.unitIdString unitid
      inplace = "-inplace" `isInfixOf` unit_string
   in getSymbols unitid inplace haddocks exposed dirs

getSymbols :: GHC.GhcMonad m => GHC.UnitId -> Bool -> [FilePath] -> Set GHC.ModuleName -> [FilePath] -> m PackageEntries
getSymbols unitid inplace haddocks exposed dirs = do
  let findHis dir = liftIO $ walkSuffix ".hi" dir
  his <- join <$> traverse findHis dirs
  dflags <- GHC.getSessionDynFlags
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
  sess <- GHC.getSession
  let unit_state = GHC.ue_units . GHC.hsc_unit_env $ sess
      findPid unitid' = GHC.unitPackageId <$> GHC.lookupUnitId unit_state unitid'
      findUnitId = GHC.toUnitId . GHC.moduleUnit
      mkPackageId (GHC.PackageId fs) = PackageId . T.pack $ GHC.unpackFS fs
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  let unit_state = GHC.unitState dflags
      findPid unitid' = GHC.unitPackageId <$> GHC.lookupUnitId unit_state unitid'
      findUnitId = GHC.toUnitId . GHC.moduleUnit
      mkPackageId (GHC.PackageId fs) = PackageId . T.pack $ GHC.unpackFS fs
#else
  let findPid unitid' = GHC.sourcePackageId <$> GHC.lookupPackage dflags unitid'
      findUnitId = GHC.moduleUnitId
      mkPackageId (GHC.SourcePackageId fs) = PackageId . T.pack $ GHC.unpackFS fs
#endif
      srcid =  findPid unitid
  symbols <- catMaybes <$> traverse (hiToSymbols exposed) his
  let entries = sort $ uncurry mkEntries <$> symbols
      mkModuleName :: GHC.Module -> ModuleName
      mkModuleName = ModuleName . T.pack . GHC.moduleNameString . GHC.moduleName
      mkEntries m things = ModuleEntries (mkModuleName m) (sort $ renderThings things)

      -- for the given module, only including the packageid if it is different
      -- than the package of the module under inspection.
      mkExported m =
        let unitid' = findUnitId m
            pid = if unitid == unitid'
                    then Nothing
                    else findPid unitid'
         in Exported (mkPackageId <$> pid) (mkModuleName m)

      renderThings things = catMaybes $ (\(mm, thing) -> tyrender dflags (mkExported <$> mm) thing) <$> things
  pure $ PackageEntries (mkPackageId <$> srcid) inplace entries (T.pack <$> haddocks)

-- for a .hi file returns the module and a list of all things (with types
-- resolved) in that module and their original module if they are re-exported.
hiToSymbols
  :: GHC.GhcMonad m
  => Set GHC.ModuleName
  -> FilePath
  -> m (Maybe (GHC.Module, [(Maybe GHC.Module, GHC.TcTyThing)]))
hiToSymbols exposed hi = (join <$>) <$> withHi hi $ \iface -> do
  let m = GHC.mi_module iface
  -- TODO we should include all modules from inplace packages, otherwise the
  -- user is unable to jump-to-definition within the same multi-package project.
  if not $ Set.member (GHC.moduleName m) exposed
    then pure Nothing
    else do
      let thing (GHC.Avail name) = traverse tcLookup' [name]
          -- TODO the fields in AvailTC
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
          thing (GHC.AvailTC _ members) = traverse tcLookup' members
#else
          thing (GHC.AvailTC _ members _) = traverse tcLookup' members
#endif
          reexport name = do
            modl <- GHC.nameModule_maybe name
            if m == modl then Nothing else Just modl
          tcLookup' name =
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
            let name' = GHC.greNameMangledName name
#else
            let name' = name
#endif
             in (reexport name',) <$> GHC.tcLookup name'
      things <- join <$> traverse thing (GHC.mi_exports iface)
      pure . Just $ (m, things)

-- TODO should we lose the dflags and use the unsafe variant?
tyrender :: GHC.DynFlags -> Maybe Exported -> GHC.TcTyThing -> Maybe Entry
tyrender dflags m (GHC.AGlobal thing) =
  let
    shw :: GHC.Outputable o => o -> Text
    shw = T.pack . GHC.showPpr dflags
    in case thing of
    (GHC.AnId var) -> Just $ IdEntry m
      (shw $ GHC.idName var)
      (shw $ GHC.idType var)
    (GHC.AConLike (GHC.RealDataCon dc)) -> Just $ ConEntry m
      (shw $ GHC.getName dc)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
      (shw $ GHC.dataConWrapperType dc)
#else
      (shw $ GHC.dataConUserType dc)
#endif
    (GHC.AConLike (GHC.PatSynCon ps)) -> Just $ PatSynEntry m
      (shw $ GHC.getName ps)
      (T.pack . GHC.showSDoc dflags $ GHC.pprPatSynType ps )
    (GHC.ATyCon tc) -> Just $ TyConEntry m
      (shw $ GHC.tyConName tc)
      (shw $ GHC.tyConFlavour tc)
    _ -> Nothing
tyrender _ _ _ = Nothing

data Entry = IdEntry (Maybe Exported) Text Text -- ^ name type
           | ConEntry (Maybe Exported) Text Text -- ^ name type
           | PatSynEntry (Maybe Exported) Text Text -- ^ name orig
           | TyConEntry (Maybe Exported) Text Text -- ^ type flavour
  deriving (Eq, Ord)
{- BOILERPLATE Entry ToSexp
   field={IdEntry:[export,name,type],
          ConEntry:[export,name,type],
          PatSynEntry:[export,name,type],
          TyConEntry:[export,type,flavour]}
   class={IdEntry:id,
          ConEntry:con,
          PatSynEntry:pat,
          TyConEntry:tycon}
-}
{- BOILERPLATE START -}
instance ToSexp Entry where
  toSexp (IdEntry p_1_1 p_1_2 p_1_3) = alist $ ("class", "id") : [("export", toSexp p_1_1), ("name", toSexp p_1_2), ("type", toSexp p_1_3)]
  toSexp (ConEntry p_1_1 p_1_2 p_1_3) = alist $ ("class", "con") : [("export", toSexp p_1_1), ("name", toSexp p_1_2), ("type", toSexp p_1_3)]
  toSexp (PatSynEntry p_1_1 p_1_2 p_1_3) = alist $ ("class", "pat") : [("export", toSexp p_1_1), ("name", toSexp p_1_2), ("type", toSexp p_1_3)]
  toSexp (TyConEntry p_1_1 p_1_2 p_1_3) = alist $ ("class", "tycon") : [("export", toSexp p_1_1), ("type", toSexp p_1_2), ("flavour", toSexp p_1_3)]
{- BOILERPLATE END -}

data ModuleEntries = ModuleEntries ModuleName [Entry]
  deriving (Eq, Ord)
{- BOILERPLATE ModuleEntries ToSexp field=[module,ids] -}
{- BOILERPLATE START -}
instance ToSexp ModuleEntries where
  toSexp (ModuleEntries p_1_1 p_1_2) = alist [("module", toSexp p_1_1), ("ids", toSexp p_1_2)]
{- BOILERPLATE END -}

-- The haddocks serve a dual purpose: not only do they point to where haddocks
-- might be, they give a hint to the text editor where the sources for this
-- package are (e.g. with the ghc distribution, build tool store or local).
--
-- Users should type `cabal haddock --enable-documentation` to populate the docs
-- of their dependencies and local projects.
type Haddocks = [Text]

-- Bool indicates if this is an -inplace package
data PackageEntries = PackageEntries (Maybe PackageId) Bool [ModuleEntries] Haddocks
{- BOILERPLATE PackageEntries ToSexp field=[srcid,inplace,modules,haddocks] -}
{- BOILERPLATE START -}
instance ToSexp PackageEntries where
  toSexp (PackageEntries p_1_1 p_1_2 p_1_3 p_1_4) = alist [("srcid", toSexp p_1_1), ("inplace", toSexp p_1_2), ("modules", toSexp p_1_3), ("haddocks", toSexp p_1_4)]
{- BOILERPLATE END -}

-- srcid is Nothing if it matches the re-export location
data Exported = Exported (Maybe PackageId) ModuleName
  deriving (Eq, Ord)

{- BOILERPLATE Exported ToSexp field=[srcid, module] -}
{- BOILERPLATE START -}
instance ToSexp Exported where
  toSexp (Exported p_1_1 p_1_2) = alist [("srcid", toSexp p_1_1), ("module", toSexp p_1_2)]
{- BOILERPLATE END -}

-- local variants of things that exist in GHC
newtype ModuleName = ModuleName Text deriving (Eq, Ord, ToSexp)
newtype PackageId = PackageId Text deriving (Eq, Ord, ToSexp)
