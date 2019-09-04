{-# LANGUAGE NamedFieldPuns #-}

module HsInspect.Search where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Maybe
import           FastString (unpackFS)
import           Finder (findExposedPackageModule)
import qualified GHC
import           GHC.PackageDb
import           HscTypes (FindResult(..))
import           HsInspect.Sexp
import           Json
import           Module (ModLocation(..), Module(..), ModuleName(..), unitIdFS)
import           PackageConfig
import           Packages (LookupResult(..), PackageConfigMap(..),
                           PackageState(..))
import           UniqDFM (udfmToList)

search :: GHC.GhcMonad m => String -> m [Hit]
search query = do
  dflags <- GHC.getSessionDynFlags
  let Just dbs = GHC.pkgDatabase dflags
      pkgs = join (snd <$> dbs)
  join <$> traverse getHits pkgs

-- TODO Maybe haddock-html
getHits :: GHC.GhcMonad m => PackageConfig -> m [Hit]
getHits pkg = do
  results <- traverse finder (lookups pkg)
  pure $ toHit <$> results

toHit :: FindResult -> Hit
toHit (Found (ModLocation _ hi _) _) = error "not implemented yet"
toHit _ = error "not supported"

finder :: GHC.GhcMonad m => LookupResult -> m FindResult
finder lup = do
  env <- GHC.getSession
  liftIO $ findLookupResult env lup

-- TODO ghc should export Finder.findLookupResult
findLookupResult :: GHC.HscEnv -> LookupResult -> IO FindResult
findLookupResult env (LookupFound (Module id name) _) =
  findExposedPackageModule env name (Just $ unitIdFS id)
findLookupResult _ _ = error "not supported"

lookups :: PackageConfig -> [LookupResult]
lookups c@InstalledPackageInfo{exposedModules} =
  let modules = catMaybes $ snd <$> exposedModules
  in flip LookupFound c <$> modules

-- LookupFound Module PackageConfig
-- findLookupResult
-- findExposedPackageModule
-- lookupIfaceByModule
-- showIface
-- readBinIface

data Hit = Hit String

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit _) = error "not implemented yet"
