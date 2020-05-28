{-# LANGUAGE RecordWildCards #-}

-- Calls to the hsinspect binary must have some context, which typically must be
-- discovered from the file that the user is currently visiting.
--
-- This module gathers the definition of the context and the logic to infer it,
-- which assumes that .cabal (or package.yaml) and .ghc.flags files are present,
-- and that the build tool is either cabal-install or stack.
module HsInspect.LSP.Context where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (withExceptT)
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
  , srcdir :: FilePath
  }

data BuildTool = Cabal | Stack

findContext :: FilePath -> BuildTool -> ExceptT String IO Context
findContext src tool = do
  ghcflags' <- discoverGhcflags src
  ghcpath' <- discoverGhcpath src
  let readWords file = words <$> readFile' file
      readFile' = liftIO . readFile
  Context <$> discoverHsInspect src tool <*> discoverPackageDir src <*> readWords ghcflags' <*> readFile' ghcpath' <*> pure (takeDirectory ghcflags')

discoverHsInspect :: FilePath -> BuildTool -> ExceptT String IO FilePath
discoverHsInspect file tool = do
  let dir = takeDirectory file
  dir' <- discoverPackageDir dir
  withExceptT (\err -> help_hsinspect ++ "\n\n" ++ err) $ case tool of
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
  failWithM "There must be a .cabal or package.yaml" $
    locateDominatingDir (\f -> isCabal f || isHpack f) dir

discoverGhcflags :: FilePath -> ExceptT String IO FilePath
discoverGhcflags file = do
  let dir = takeDirectory file
  failWithM ("There must be a .ghc.flags file. " ++ help_ghcflags) $
   locateDominatingFile (".ghc.flags" ==) dir

discoverGhcpath :: FilePath -> ExceptT String IO FilePath
discoverGhcpath file = do
  let dir = takeDirectory file
  failWithM ("There must be a .ghc.path file. " ++ help_ghcflags) $
    locateDominatingFile (".ghc.path" ==) dir

-- note that any formatting in these messages are stripped
help_ghcflags :: String
help_ghcflags = "The cause of this error could be that this package has not been compiled yet, \
                \or the ghcflags compiler plugin has not been installed for this package. \
                \See https://gitlab.com/tseenshe/hsinspect#installation for more details."

help_hsinspect :: String
help_hsinspect = "The hsinspect binary has not been installed for this package. \
                 \See https://gitlab.com/tseenshe/hsinspect#installation for more details."

-- from Control.Error.Util
failWithM :: Applicative m => e -> m (Maybe a) -> ExceptT e m a
failWithM e ma = ExceptT $ (maybe (Left e) Right) <$> ma
