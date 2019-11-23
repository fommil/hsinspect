{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Packages (packages, PkgSummary) where

import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort, (\\))
import Data.Maybe (catMaybes)
import FastString
import Finder (findImportedModule)
import qualified GHC
import HscTypes (FindResult(..))
import HsInspect.Sexp
import HsInspect.Util
import HsInspect.Workarounds
import Module (Module(..), ModuleName, unitIdString)
import Packages (PackageState(..))
import qualified RdrName as GHC

-- Similar to packunused / weeder, but more reliable (and doesn't require a
-- separate -ddump-minimal-imports pass).
--
-- TODO get the dirs from the dynflags not the user
packages :: GHC.GhcMonad m => FilePath -> m PkgSummary
packages dir = do
  -- We load all .hs files in dir, assuming they are the sources of the home
  -- module, but with a twist: we only parse the imports from external packages.
  -- To do this we have to unload the home modules as provided by parameters and
  -- filter then when parsing the imports section.
  homes <- getTargetModules

  srcs <- liftIO $ walkSuffix ".hs" dir

  -- mods == homes. We could do two passes (ignore provided targets)
  (catMaybes -> mods, targets) <- unzip <$> traverse (importsOnly homes) srcs
  _ <- GHC.setTargets targets

  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags { GHC.ghcMode = GHC.CompManager }
  _ <- GHC.load $ GHC.LoadAllTargets

  imps <- nub . join <$> traverse getImports mods
  pkgs <- catMaybes <$> traverse (uncurry findPackage) imps
  let used = nub . sort $ pkgs
  let loaded = nub . sort . explicitPackages $ GHC.pkgState dflags
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
    where toS ids = toSexp $ unitIdString <$> ids
