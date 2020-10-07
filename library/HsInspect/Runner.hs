{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Runner (runGhcAndJamMasterShe, ghcflags_flags) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Data.List (find, isPrefixOf)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import DynFlags (parseDynamicFlagsCmdLine, updOptLevel)
import qualified EnumSet as EnumSet
import GHC (Ghc, GhcMonad, getSessionDynFlags)
import qualified GHC as GHC
import HsInspect.Context
import HsInspect.Util (homeSources)
import HsInspect.Workarounds (parseModuleName')
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (setEnv)

-- expects the PWD to be the same as the .cabal file and the PATH to be what the
-- build tool sees.
runGhcAndJamMasterShe :: [String] -> Ghc a -> IO a
runGhcAndJamMasterShe (filterFlags -> flags) work =
  let libdir = (drop 2) <$> find ("-B" `isPrefixOf`) flags
      flags' = filter (not . ("-B" `isPrefixOf`)) flags
   in GHC.runGhc libdir $ do
  dflags <- GHC.getSessionDynFlags
  (updOptLevel 0 -> dflags', (GHC.unLoc <$>) -> _ghcargs, _) <-
    liftIO $ parseDynamicFlagsCmdLine dflags (GHC.noLoc <$> flags')
  void $ GHC.setSessionDynFlags dflags'
         { GHC.hscTarget = GHC.HscInterpreted -- HscNothing compiles home modules, dunno why
         , GHC.ghcLink   = GHC.LinkInMemory   -- required by HscInterpreted
         , GHC.ghcMode   = GHC.MkDepend       -- prefer .hi to .hs for dependencies
         , GHC.warningFlags = EnumSet.empty
         , GHC.fatalWarningFlags = EnumSet.empty
         }

  -- The caller may have provided a list of home modules, but we do not trust
  -- them because the ghcflags plugin does not keep the flags up to date for
  -- incremental compiles.
  let mkTarget m = GHC.Target (GHC.TargetModule m) True Nothing
  homeModules <- inferHomeModules
  GHC.setTargets $ mkTarget <$> homeModules
  work

-- gets the flags (and sets the environment) from the output of the ghcflags plugin
ghcflags_flags :: Maybe FilePath -> ExceptT String IO [String]
ghcflags_flags mf = do
  from <- liftIO $ maybe getCurrentDirectory pure mf
  Context{package_dir, ghcflags, ghcpath} <- findContext from
  liftIO $ do
    setCurrentDirectory package_dir
    setEnv "PATH" (T.unpack ghcpath)
  pure $ T.unpack <$> ghcflags

inferHomeModules :: GHC.GhcMonad m => m [GHC.ModuleName]
inferHomeModules = do
  files <- homeSources
  mmns <- traverse parseModuleName' files
  let main' = GHC.mkModuleName "Main"
  pure . L.nub . filter (main' /=) $ catMaybes mmns
  -- stack often has duplicates

-- removes the "+RTS ... -RTS" sections
filterFlags :: [String] -> [String]
filterFlags args = case span ("+RTS" /=) args of
  (front, []) -> front
  (front, _ : middle) -> case span ("-RTS" /=) middle of
    (_, []) -> front -- bad input?
    (_, _ : back) -> front <> back

