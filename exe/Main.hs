{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Config as GHC
import Control.Monad
import Control.Monad.IO.Class
import Data.List (find, isPrefixOf)
import Data.Maybe (catMaybes)
import DynFlags (parseDynamicFlagsCmdLine, updOptLevel)
import qualified EnumSet as EnumSet
import qualified GHC as GHC
import HsInspect.Imports
import HsInspect.Index
import HsInspect.Json
import HsInspect.Packages
import HsInspect.Sexp as S
import HsInspect.Util
import HsInspect.Workarounds
import System.Environment (getArgs)
import System.Exit

version :: String
#ifdef CURRENT_PACKAGE_VERSION
version = CURRENT_PACKAGE_VERSION
#else
version = "unknown"
#endif

help :: String
help =
  "hsinspect command ARGS [--json|help|version|ghc-version] -- [ghcflags]\n\n" ++
  "  `command ARGS' can be:\n\n" ++
  "  imports /path/to/file.hs - list the qualified imports for the file\n" ++
  "                             along with their locally qualified (and\n" ++
  "                             unqualified) names.\n" ++
  "  index                    - list all dependency packages, modules, terms and types.\n" ++
  "  packages /path/to/dir    - list all packages that are referenced by sources in this dir.\n"

-- Possible backends:
--
-- https://github.com/mpickering/hie-bios
-- http://hackage.haskell.org/package/cabal-helper
main :: IO ()
main = do
  (break ("--" ==) -> (args, filterFlags -> flags)) <- getArgs
  when (elem "--help" args) $
    (putStrLn help) >> exitWith ExitSuccess
  when (elem "--version" args) $
    (putStrLn version) >> exitWith ExitSuccess
  when (elem "--ghc-version" args) $
    (putStrLn GHC.cProjectVersion) >> exitWith ExitSuccess
  let libdir = (drop 2) <$> find ("-B" `isPrefixOf`) flags
      flags' = filter (not . ("-B" `isPrefixOf`)) flags
  GHC.runGhc libdir $ do
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

    let respond rest (S.filterNil . S.toSexp -> a) = liftIO . putStrLn $
          if (elem "--json" rest)
          then case sexpToJson a of
            Left err -> error err
            Right j -> encodeJson dflags' j
          else S.render a
    case args of
      "imports" : file : rest -> do
        quals <- imports file
        respond rest quals
      "index" : rest -> do
        hits <- index
        respond rest hits
      "packages" : rest -> do
        hits <- packages
        respond rest hits
      _ ->
        liftIO $ error "invalid parameters"

inferHomeModules :: GHC.GhcMonad m => m [GHC.ModuleName]
inferHomeModules = do
  files <- homeSources
  catMaybes <$> traverse parseModuleName' files

-- removes the "+RTS ... -RTS" sections
filterFlags :: [String] -> [String]
filterFlags args = case span ("+RTS" /=) args of
  (front, []) -> front
  (front, _ : middle) -> case span ("-RTS" /=) middle of
    (_, []) -> front -- bad input?
    (_, _ : back) -> front <> back
