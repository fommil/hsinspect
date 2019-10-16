{-# LANGUAGE NamedFieldPuns #-}

-- | Calculate all exposed modules that could be imported.
module HsInspect.Modules
  ( modules,
    Hit,
  )
where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC
import GHC.PackageDb
import HsInspect.Sexp
import Json
import Module (UnitId)
import PackageConfig
import Packages (explicitPackages)

modules :: GHC.GhcMonad m => [String] -> m [Hit]
modules homeModules = do
  -- TODO where is base?
  dflags <- GHC.getSessionDynFlags
  let Just dbs = GHC.pkgDatabase dflags
      loaded = Set.fromList . explicitPackages $ GHC.pkgState dflags
      home = Hit <$> homeModules
      away = (mods loaded =<<) =<< (snd <$> dbs)
  pure . sort $ home <> away

mods :: Set UnitId -> PackageConfig -> [Hit]
mods allowed p =
  if Set.notMember (packageConfigId p) allowed
    then []
    else Hit . GHC.moduleNameString . fst <$> exposedModules p

data Hit = Hit String
  deriving (Eq, Ord)

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit txt) = JSString txt
