{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Config as GHC
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isUpper)
import Data.List (find, isPrefixOf)
import DynFlags (parseDynamicFlagsCmdLine, updOptLevel)
import qualified GHC as GHC
import HsInspect.Imports
import HsInspect.Index
import HsInspect.Json
import HsInspect.Packages
import HsInspect.Sexp as S
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
  GHC.runGhc libdir $ do
    dflags <- GHC.getSessionDynFlags
    (updOptLevel 0 -> dflags', (GHC.unLoc <$>) -> ghcargs, _) <-
      liftIO $ parseDynamicFlagsCmdLine dflags (GHC.noLoc <$> flags)
    void $ GHC.setSessionDynFlags dflags'
           { GHC.hscTarget = GHC.HscInterpreted -- HscNothing compiles home modules, dunno why
           , GHC.ghcLink   = GHC.LinkInMemory   -- required by HscInterpreted
           , GHC.ghcMode   = GHC.MkDepend       -- prefer .hi to .hs for dependencies
           }
    let homeModules = (filter (isUpper . head) ghcargs)
    GHC.setTargets $
      -- TODO it would be good if ghc had a "binary only" option for Targets
      --      with fail fast if only source code (no .hi) is discovered.
      (\m -> GHC.Target (GHC.TargetModule $ GHC.mkModuleName m) True Nothing) <$> homeModules
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
      "packages" : dir : rest -> do
        hits <- packages dir
        respond rest hits
      _ ->
        liftIO $ error "invalid parameters"

-- TODO let each component remove things that interfere
filterFlags :: [String] -> [String]
filterFlags ("--" : rest) = filter allow rest
  where allow flag = "-Wno" `isPrefixOf` flag || not ("-W" `isPrefixOf` flag)
filterFlags _ = []
