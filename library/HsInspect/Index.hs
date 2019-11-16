{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dumps an index of all terms and their types
module HsInspect.Index
  ( index,
    PackageEntries,
  )
where

import Avail (AvailInfo(..))
import BinIface (CheckHiWay(..), TraceBinIFaceReading(..), readBinIface)
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC
import GHC.PackageDb
import HscTypes (ModIface(..))
import HsInspect.Sexp
import HsInspect.Util
import qualified Id as GHC
import Json
import Module (Module(..), moduleNameString, unitIdString)
import Outputable (showPpr)
import PackageConfig
--import System.IO (hPutStrLn, stderr)
import PackageConfig (packageConfigId)
import Packages (explicitPackages, lookupPackage)
import TcEnv (tcLookup)
import TcRnMonad (initTcInteractive)
import qualified TcRnTypes as GHC

index :: GHC.GhcMonad m => m [PackageEntries]
index = do
  -- TODO the home package
  dflags <- GHC.getSessionDynFlags
  let explicit = explicitPackages $ GHC.pkgState dflags
      pkgcfgs = maybeToList . lookupPackage dflags =<< explicit
  traverse getSymbols pkgcfgs

-- TODO Maybe haddock-html
-- TODO Maybe source definition (or should we leave source resolution to downstream?)
getSymbols :: GHC.GhcMonad m => PackageConfig -> m PackageEntries
getSymbols pkg = do
  let findHis dir = filter (".hi" `isSuffixOf`) <$> liftIO (walk dir)
      exposed = Set.fromList $ fst <$> exposedModules pkg
      unitid = packageConfigId pkg
  his <- join <$> traverse findHis (importDirs pkg)
  dflags <- GHC.getSessionDynFlags
  symbols <- catMaybes <$> traverse (hiToSymbols exposed) his
  let entries = uncurry mkEntries <$> symbols
      mkEntries m things = ModuleEntries (moduleName m) (renderThings things)
      renderThings things = catMaybes $ (tyrender dflags) <$> things
  pure $ PackageEntries unitid entries

hiToSymbols :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m (Maybe (GHC.Module, [GHC.TcTyThing]))
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
          let thing (Avail name) = traverse tcLookup [name]
              -- TODO the fields in AvailTC
              thing (AvailTC name members _) = traverse tcLookup (name : members)
          things <- traverse thing (mi_exports iface)
          pure . Just $ (m, join things)
  pure $ join hits

tyrender :: GHC.DynFlags -> GHC.TcTyThing -> Maybe Entry
tyrender dflags (GHC.AGlobal (GHC.AnId var)) =
  Just
    $ Entry (showPpr dflags $ GHC.idName var)
        (showPpr dflags $ GHC.idType var)
-- TODO investigate what we're skipping
tyrender _ _ = Nothing

-- TODO normalise the type string to make it easier for downstream tools to perform searches
-- TODO note if this is the original definition point (vs a re-export)
data Entry = Entry String String

data ModuleEntries = ModuleEntries GHC.ModuleName [Entry]

data PackageEntries = PackageEntries GHC.UnitId [ModuleEntries]

instance ToSexp Entry where
  toSexp (Entry term typ) =
    alist
      [ ("name", SexpString term),
        ("type", SexpString typ)
      ]

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

instance ToJson Entry where
  json (Entry term typ) =
    JSObject
      [ ("name", JSString term),
        ("type", JSString typ)
      ]

instance ToJson ModuleEntries where
  json (ModuleEntries modl entries) =
    JSObject
      [ ("module", JSString . moduleNameString $ modl),
        ("ids", JSArray $ json <$> entries)
      ]

instance ToJson PackageEntries where
  json (PackageEntries pkg modules) =
    JSObject
      [ ("unitid", JSString . unitIdString $ pkg),
        ("modules", JSArray $ json <$> modules)
      ]
