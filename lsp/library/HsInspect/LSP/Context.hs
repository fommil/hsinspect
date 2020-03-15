{-# LANGUAGE RecordWildCards #-}

-- Calls to the hsinspect binary must have some context, which typically must be
-- discovered from the file that the user is currently visiting.
--
-- This module gathers the definition of the context and the logic to infer it,
-- which assumes that .cabal (or package.yaml) and .ghc.flags files are present,
-- and that the build tool is either cabal-install or stack.
module HsInspect.LSP.Context where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.List (isSuffixOf)
import Data.List.Extra (trim)
import HsInspect.LSP.Util
import System.Directory (setCurrentDirectory)
import System.FilePath

data Context = Context
  { hsinspect :: FilePath
  , package_dir :: FilePath
  , ghcflags :: [String]
  }

data DiscoverContext m = DiscoverContext
  { discoverHsInspect :: FilePath -> m FilePath
  , discoverPackageDir :: FilePath -> m FilePath
  , discoverGhcflags :: FilePath -> m FilePath
  }

mkDiscoverContext :: DiscoverContext ZIO
mkDiscoverContext = DiscoverContext {..}
  where
    discoverHsInspect :: FilePath -> ZIO FilePath
    discoverHsInspect file = do
      let dir = takeDirectory file
      dir' <- discoverProjectDir dir <|> discoverPackageDir dir
      lift $ setCurrentDirectory dir'
      -- TODO abstract over build tool
      -- stack build --silent hsinspect && stack exec --silent which -- hsinspect
      _ <- shell "cabal" ["build", "-v0", ":pkg:hsinspect:exe:hsinspect"]
      trim <$> shell "cabal" ["exec", "--silent", "which", "--", "hsinspect"]

    -- c.f. haskell-tng--compile-dominating-project
    discoverProjectDir :: FilePath -> ZIO FilePath
    discoverProjectDir file = do
      let dir = takeDirectory file
          files = ["cabal.project", "cabal.project.local", "cabal.project.freeze", "stack.yaml"]
      locateDominating (flip elem files) dir

    -- c.f. haskell-tng--compile-dominating-package
    discoverPackageDir :: FilePath -> ZIO FilePath
    discoverPackageDir file = do
      let dir = takeDirectory file
          isCabal = (".cabal" `isSuffixOf`)
          isHpack = ("package.yaml" ==)
      locateDominating (\f -> isCabal f || isHpack f) dir

    discoverGhcflags :: FilePath -> ZIO FilePath
    discoverGhcflags file = do
      let dir = takeDirectory file
      (</> ".ghc.flags") <$> locateDominating (".ghc.flags" ==) dir
