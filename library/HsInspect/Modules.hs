{-# LANGUAGE NamedFieldPuns #-}

-- | Calculate all exposed modules that could be imported.
module HsInspect.Modules(modules) where

import           Data.List (sort)
import qualified GHC
import           GHC.PackageDb
import           HsInspect.Sexp
import           Json
import           PackageConfig

-- TODO package modules, add the package-id

-- FIXME seems to include more modules that in the deps
-- (could be a tests.sh bug)

modules :: GHC.GhcMonad m => [String] -> m [Hit]
modules homeModules = do
  dflags <- GHC.getSessionDynFlags
  let Just dbs = GHC.pkgDatabase dflags
      home = Hit <$> homeModules
      away = (mods =<<) =<< (snd <$> dbs)
  pure . sort $ home <> away

-- TODO filter by exposed packages
mods :: PackageConfig -> [Hit]
mods InstalledPackageInfo{exposedModules} =
  Hit . GHC.moduleNameString . fst <$> exposedModules

data Hit = Hit String
  deriving (Eq, Ord)

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit txt) = JSString txt
