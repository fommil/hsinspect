{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Search
  ( search,
    Hit,
  )
where

import BinIface
  ( CheckHiWay (..),
    TraceBinIFaceReading (..),
    readBinIface,
  )
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC
import GHC.PackageDb
import HsInspect.Sexp
import HsInspect.Util
import HscTypes (ModIface (..))
import Json
import qualified Name as GHC
import PackageConfig
import Packages (explicitPackages)
import TcRnMonad (initTcRnIf)

search :: GHC.GhcMonad m => String -> m [Hit]
search _query = do
  -- TODO support home modules
  -- TODO where is base?
  dflags <- GHC.getSessionDynFlags

  -- TODO this logic is used in Packages / Modules, share
  let Just ((snd =<<) -> allPkgs) = GHC.pkgDatabase dflags
      explicit = Set.fromList . explicitPackages $ GHC.pkgState dflags
      pkgs = filter (\(packageConfigId -> pid) -> Set.member pid explicit) allPkgs
  join <$> traverse getSymbols pkgs
  -- ^ TODO filter and rank the symbols by the search

-- TODO Maybe haddock-html
getSymbols :: GHC.GhcMonad m => PackageConfig -> m [Hit]
getSymbols pkg = do
  let findHis dir = filter (".hi" `isSuffixOf`) <$> liftIO (walk dir)
      exposed = Set.fromList $ fst <$> exposedModules pkg
  his <- join <$> traverse findHis (importDirs pkg)
  join <$> traverse (hiToSymbols exposed) his

hiToSymbols :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m [Hit]
hiToSymbols exposed hi = do
  env <- GHC.getSession
  iface <-
    liftIO $ initTcRnIf 'z' env () ()
      $ readBinIface IgnoreHiWay QuietBinIFaceReading hi
  let m = mi_module iface
  pure
    $ if not $ Set.member (GHC.moduleName m) exposed
      then []
      else do
        -- FIXME the Name is not very useful, use loadDecls
        decl <- GHC.getName . snd <$> mi_decls iface
        pure $ Hit m decl

data Hit = Hit GHC.Module GHC.Name

instance ToSexp Hit where
  toSexp (Hit _ name) = toSexp . GHC.getOccString $ name

instance ToJson Hit where
  json (Hit _ name) = JSString . GHC.getOccString $ name
