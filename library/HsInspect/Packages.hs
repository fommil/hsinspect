{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HsInspect.Packages (packages, PkgSummary) where

import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete, nub, sort, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified DynFlags as GHC
import FastString
import Finder (findImportedModule)
import qualified GHC
import HscTypes (FindResult(..))
import HsInspect.Sexp
import HsInspect.Util
import HsInspect.Workarounds
import Module (Module(..), ModuleName)
import Packages (PackageState(..))
import qualified RdrName as GHC

-- Similar to packunused / weeder, but more reliable (and doesn't require a
-- separate -ddump-minimal-imports pass).
packages :: GHC.GhcMonad m => m PkgSummary
packages = do
  mods <- Set.toList <$> getTargetModules

  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags { GHC.ghcMode = GHC.CompManager }
  _ <- GHC.load $ GHC.LoadAllTargets

  imps <- nub . join <$> traverse getImports mods
  pkgs <- catMaybes <$> traverse (uncurry findPackage) imps
  let home = GHC.thisPackage dflags
      used = delete home . nub . sort $ pkgs
      loaded = nub . sort . explicitPackages $ GHC.pkgState dflags
  pure $ PkgSummary used (loaded \\ used)

findPackage :: GHC.GhcMonad m => ModuleName -> Maybe FastString -> m (Maybe GHC.UnitId)
findPackage m mp = do
  env <- GHC.getSession
  res <- liftIO $ findImportedModule env m mp
  pure $ case res of
    Found _ (Module u _) -> Just $ u
    _ -> Nothing

getImports :: GHC.GhcMonad m => ModuleName -> m [(ModuleName, Maybe FastString)]
getImports m = do
  rdr_env <- minf_rdr_env' m
  let imports = GHC.gre_imp =<< GHC.globalRdrEnvElts rdr_env
  pure $ qModule <$> imports

-- PackageImports are not supported until ImpDeclSpec supports them (could parse
-- gre_name's src span if we're desperate)
qModule :: GHC.ImportSpec -> (ModuleName, Maybe FastString)
qModule (GHC.ImpSpec (GHC.ImpDeclSpec{GHC.is_mod}) _) = (is_mod, Nothing)

data PkgSummary = PkgSummary [GHC.UnitId] [GHC.UnitId]
  deriving (Eq, Ord)

instance ToSexp PkgSummary where
  toSexp (PkgSummary used unused) =
    alist [ ("used", toS used)
          , ("unused", toS unused) ]
    where toS ids = toSexp $ normaliseUnitId <$> ids
