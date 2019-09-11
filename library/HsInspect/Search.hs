{-# LANGUAGE NamedFieldPuns #-}

module HsInspect.Search where

import           BinIface (CheckHiWay(..), TraceBinIFaceReading(..),
                           readBinIface)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Finder (findExposedPackageModule)
import qualified GHC
import           GHC.PackageDb
import           HscTypes (FindResult(..), ModIface(..))
import           HsInspect.Sexp
import           Json
import           Module (ModLocation(..), Module(..), unitIdFS)
import           PackageConfig
import           Packages (LookupResult(..))
import           TcRnMonad (initTcRnIf)

search :: GHC.GhcMonad m => String -> m [Hit]
search _query = do
  dflags <- GHC.getSessionDynFlags
  let Just dbs = GHC.pkgDatabase dflags
      pkgs = join (snd <$> dbs)
  join <$> traverse getHits pkgs

-- TODO Maybe haddock-html
getHits :: GHC.GhcMonad m => PackageConfig -> m [Hit]
getHits pkg = do
  results <- traverse finder (lookups pkg)
  join <$> traverse toHit results

-- FIXME finder doesn't work. It probably needs everything to be loaded. Just
-- traverse the library directories looking for .hi files and filter the modules
-- based on the exported list. That should avoid the risk of compiling anything.

toHit :: GHC.GhcMonad m => FindResult -> m [Hit]
toHit (Found (ModLocation _ hi _) _) = do
  env <- GHC.getSession
  iface <- liftIO $ initTcRnIf 'z' env () () $
    readBinIface IgnoreHiWay QuietBinIFaceReading hi
  pure [Hit . show . length $ mi_exports iface]
toHit _ = pure []

-- LookupFound Module PackageConfig
-- findLookupResult
-- findExposedPackageModule
-- lookupIfaceByModule
-- showIface
-- readBinIface

finder :: GHC.GhcMonad m => LookupResult -> m FindResult
finder lup = do
  env <- GHC.getSession
  liftIO $ findLookupResult env lup

-- TODO ghc should export Finder.findLookupResult
findLookupResult :: GHC.HscEnv -> LookupResult -> IO FindResult
findLookupResult env (LookupFound (Module mid name) _) =
  findExposedPackageModule env name (Just $ unitIdFS mid)
findLookupResult _ _ = error "not supported"

lookups :: PackageConfig -> [LookupResult]
lookups c@InstalledPackageInfo{exposedModules} =
  let modules = catMaybes $ snd <$> exposedModules
  in flip LookupFound c <$> modules

data Hit = Hit String

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit _) = error "not implemented yet"
