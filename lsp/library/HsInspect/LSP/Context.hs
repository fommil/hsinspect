{-# LANGUAGE RecordWildCards #-}

-- Calls to the hsinspect binary must have some context, which typically must be
-- discovered from the file that the user is currently visiting.
--
-- This module gathers the definition of the context and the logic to infer it,
-- which assumes that .cabal (or package.yaml) and .ghc.flags files are present,
-- and that the build tool is either cabal-install or stack.
module HsInspect.LSP.Context where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.List (isSuffixOf)
import Data.List.Extra (trim)
import HsInspect.LSP.Util
import System.FilePath

data Context = Context
  { hsinspect :: FilePath
  , package_dir :: FilePath
  , ghcflags :: [String]
  , ghcpath :: String
  }

findContext :: MonadIO m => DiscoverContext m -> FilePath -> m Context
findContext DiscoverContext{..} src = do
  ghcflags' <- discoverGhcflags src
  ghcpath' <- discoverGhcpath src
  let readWords file = words <$> readFile' file
      readFile' = liftIO . readFile
  Context <$> discoverHsInspect src <*> discoverPackageDir src <*> readWords ghcflags' <*> readFile' ghcpath'

data DiscoverContext m = DiscoverContext
  { discoverHsInspect :: FilePath -> m FilePath
  , discoverPackageDir :: FilePath -> m FilePath
  , discoverGhcflags :: FilePath -> m FilePath
  , discoverGhcpath :: FilePath -> m FilePath
  }

data BuildTool = Cabal | Stack

mkDiscoverContext :: BuildTool -> DiscoverContext (ExceptT String IO)
mkDiscoverContext tool = DiscoverContext {..}
  where
    discoverHsInspect :: FilePath -> ExceptT String IO FilePath
    discoverHsInspect file = do
      let dir = takeDirectory file
      dir' <- discoverPackageDir dir
      case tool of
        Cabal -> do
          _ <- shell "cabal" ["build", "-v0", ":pkg:hsinspect:exe:hsinspect"] (Just dir') Nothing []
          trim <$> shell "cabal" ["exec", "-v0", "which", "--", "hsinspect"] (Just dir') Nothing []
        Stack -> do
          _ <- shell "stack" ["build", "--silent", "hsinspect"] (Just dir') Nothing []
          trim <$> shell "stack" ["exec", "--silent", "which", "--", "hsinspect"] (Just dir') Nothing []

    -- c.f. haskell-tng--compile-dominating-package
    discoverPackageDir :: FilePath -> ExceptT String IO FilePath
    discoverPackageDir file = do
      let dir = takeDirectory file
          isCabal = (".cabal" `isSuffixOf`)
          isHpack = ("package.yaml" ==)
      locateDominating (\f -> isCabal f || isHpack f) dir

    discoverGhcflags :: FilePath -> ExceptT String IO FilePath
    discoverGhcflags file = do
      let dir = takeDirectory file
      (</> ".ghc.flags") <$> locateDominating (".ghc.flags" ==) dir

    discoverGhcpath :: FilePath -> ExceptT String IO FilePath
    discoverGhcpath file = do
      let dir = takeDirectory file
      (</> ".ghc.path") <$> locateDominating (".ghc.path" ==) dir
