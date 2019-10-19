{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dumps an index of all terms and their types
module HsInspect.Index
  ( index,
    Hit,
  )
where

import Avail (AvailInfo (..))
import BinIface
  ( CheckHiWay (..),
    TraceBinIFaceReading (..),
    readBinIface,
  )
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC
import GHC.PackageDb
import HsInspect.Sexp
import HsInspect.Util
import HscTypes (ModIface (..))
import qualified Id as GHC
import Json
import Module (Module (..), moduleNameString, unitIdString)
import Outputable (showPpr)
import PackageConfig
import Packages (explicitPackages, lookupPackage)
--import System.IO (hPutStrLn, stderr)
import TcEnv (tcLookup)
import TcRnMonad (initTcInteractive)
import qualified TcRnTypes as GHC

index :: GHC.GhcMonad m => m [Hit]
index = do
  -- TODO the home package
  dflags <- GHC.getSessionDynFlags
  let explicit = explicitPackages $ GHC.pkgState dflags
      pkgcfgs = maybeToList . lookupPackage dflags =<< explicit
  join <$> traverse getSymbols pkgcfgs

-- TODO Maybe haddock-html
-- TODO Maybe source definition (or should we leave source resolution to downstream?)
getSymbols :: GHC.GhcMonad m => PackageConfig -> m [Hit]
getSymbols pkg = do
  let findHis dir = filter (".hi" `isSuffixOf`) <$> liftIO (walk dir)
      exposed = Set.fromList $ fst <$> exposedModules pkg
  his <- join <$> traverse findHis (importDirs pkg)
  join <$> traverse (hiToSymbols exposed) his

hiToSymbols :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m [Hit]
hiToSymbols exposed hi = do
  env <- GHC.getSession
  dflags <- GHC.getSessionDynFlags
  (_, hits) <-
    -- TODO use initTc instead of initTcInteractive
    liftIO . initTcInteractive env $ do
      iface <- readBinIface IgnoreHiWay QuietBinIFaceReading hi
      let m = mi_module iface
      if not $ Set.member (GHC.moduleName m) exposed
        then pure []
        else do
          let thing (Avail name) = traverse tcLookup [name]
              -- TODO the fields in AvailTC
              thing (AvailTC name members _) = traverse tcLookup (name : members)
          things <- join <$> traverse thing (mi_exports iface)
          pure $ (\t -> maybeToList $ (uncurry $ Hit m) <$> tyrender dflags t) =<< things
  pure $ concat hits

-- TODO normalise the type string to make it easier for downstream tools to perform searches
tyrender :: GHC.DynFlags -> GHC.TcTyThing -> Maybe (String, String)
tyrender dflags (GHC.AGlobal (GHC.AnId var)) = Just (showPpr dflags $ GHC.idName var, showPpr dflags $ GHC.idType var)
tyrender _ _ = Nothing

-- TODO investigate what we're skipping

-- TODO optimise the output by grouping by unitid / module
data Hit = Hit GHC.Module String String

instance ToSexp Hit where
  toSexp (Hit modl name typ) =
    alist
      [ ("module", SexpString . moduleNameString . moduleName $ modl),
        ("unitid", SexpString . unitIdString . moduleUnitId $ modl),
        ("name", SexpString name),
        ("type", SexpString typ)
      ]

instance ToJson Hit where
  json (Hit modl name typ) =
    JSObject
      [ ("module", JSString . moduleNameString . moduleName $ modl),
        ("unitid", JSString . unitIdString . moduleUnitId $ modl),
        ("name", JSString name),
        ("type", JSString typ)
      ]
