-- Calls to the hsinspect binary must have some context, which typically must be
-- discovered from the file that the user is currently visiting.
--
-- This module gathers the definition of the context and the logic to infer it,
-- which assumes that .cabal (or package.yaml) and .ghc.flags files are present.
module HsInspect.Context where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import HsInspect.Util (locateDominating, locateDominatingDir)
import System.FilePath

data Context = Context
  { package_dir :: FilePath
  , ghcflags :: [Text]
  , ghcpath :: Text
  , srcdir :: FilePath
  }

findContext :: FilePath -> ExceptT String IO Context
findContext src = do
  ghcflags' <- discoverGhcflags src
  ghcpath' <- discoverGhcpath src
  let readWords file = T.words <$> readFile' file
      readFile' = liftIO . T.readFile
  ghcpath <- readFile' ghcpath'
  Context <$> discoverPackageDir src <*> readWords ghcflags' <*> pure ghcpath <*> pure (takeDirectory ghcflags')

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
   locateDominating (".ghc.flags" ==) dir

discoverGhcpath :: FilePath -> ExceptT String IO FilePath
discoverGhcpath file = do
  let dir = takeDirectory file
  failWithM ("There must be a .ghc.path file. " ++ help_ghcflags) $
    locateDominating (".ghc.path" ==) dir

-- note that any formatting in these messages are stripped
help_ghcflags :: String
help_ghcflags = "The cause of this error could be that this package has not been compiled yet, \
                \or the ghcflags compiler plugin has not been installed for this package. \
                \See https://gitlab.com/tseenshe/hsinspect#installation for more details."

-- from Control.Error.Util
failWithM :: Applicative m => e -> m (Maybe a) -> ExceptT e m a
failWithM e ma = ExceptT $ (maybe (Left e) Right) <$> ma
