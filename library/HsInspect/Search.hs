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
import qualified Data.Set as Set
import qualified GHC
import GHC.PackageDb
import HsInspect.Sexp
import HsInspect.Util
import HscTypes (ModIface (..))
import Json
import PackageConfig
import Packages (explicitPackages)
import TcRnMonad (initTcRnIf)

search :: GHC.GhcMonad m => String -> m [Hit]
search query = do
  liftIO . putStrLn . show $ "SEARCH: " <> query
  -- TODO support home modules
  dflags <- GHC.getSessionDynFlags

  -- TODO this logic is used in Packages / Modules, share
  let Just ((snd =<<) -> allPkgs) = GHC.pkgDatabase dflags
      explicit = Set.fromList . explicitPackages $ GHC.pkgState dflags
      pkgs = filter (\(packageConfigId -> pid) -> Set.member pid explicit) allPkgs
  symbols <- join <$> traverse getSymbols pkgs
  pure $ Hit <$> symbols
  -- ^ TODO filter and rank the symbols by the search

-- TODO Maybe haddock-html
getSymbols :: GHC.GhcMonad m => PackageConfig -> m [String]
getSymbols pkg = do
  let findHis dir = do
        liftIO . putStrLn . show $ "WALKING: " <> dir
        filter (".hi" `isSuffixOf`) <$> liftIO (walk dir)
  his <- join <$> traverse findHis (importDirs pkg)
  join <$> traverse hiToSymbols his

-- TODO filter out hidden modules
hiToSymbols :: GHC.GhcMonad m => FilePath -> m [String]
hiToSymbols hi = do
  liftIO . putStrLn . show $ "PARSING: " <> hi
  env <- GHC.getSession
  iface <-
    liftIO $ initTcRnIf 'z' env () ()
      $ readBinIface IgnoreHiWay QuietBinIFaceReading hi
  pure [show . length $ mi_exports iface]

data Hit = Hit String

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit txt) = JSString txt
