{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Config as GHC
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import DynFlags (unsafeGlobalDynFlags)
import HsInspect.Imports
import HsInspect.Index
import HsInspect.Json
import HsInspect.Packages
import HsInspect.Runner
import HsInspect.Sexp as S
import HsInspect.Types
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
  "hsinspect command ARGS [--json|ghcflags|help|version|ghc-version] -- [ghcflags]\n\n" ++
  "  `command ARGS' can be:\n\n" ++
  "  imports /path/to/file.hs - list the qualified imports for the file\n" ++
  "                             along with their locally qualified (and\n" ++
  "                             unqualified) names.\n" ++
  "  index                    - list all dependency packages, modules, terms and types.\n" ++
  "  packages /path/to/dir    - list all packages that are referenced by sources in this dir.\n\n" ++
  " If --ghcflags is used, the flags and path will be automatically inferred from\n" ++
  " .ghc.flags and .ghc.path files based on the file and current directory. Otherwise the\n" ++
  " PATH, PWD and ghcflags must be provided."

main :: IO ()
main = do
  (break ("--" ==) -> (args, explicit_flags)) <- getArgs
  when (elem "--help" args) $
    (putStrLn help) >> exitWith ExitSuccess
  when (elem "--version" args) $
    (putStrLn version) >> exitWith ExitSuccess
  when (elem "--ghc-version" args) $
    (putStrLn GHC.cProjectVersion) >> exitWith ExitSuccess

  flags <- if (elem "--ghcflags" args)
           then ghcflags_flags $ case args of
             "imports" : file : _ -> Just file
             "types" : file : _ -> Just file
             _ -> Nothing
           else pure explicit_flags

  let respond rest (S.filterNil . S.toSexp -> a) = liftIO $
        if (elem "--json" rest)
        then case sexpToJson a of
          Left err -> error err
          Right j -> putStrLn $ encodeJson unsafeGlobalDynFlags j
        else T.putStrLn $ S.render a

  runGhcAndJamMasterShe flags $ case args of
    "imports" : file : rest -> do
      quals <- imports file
      respond rest quals
    "index" : rest -> do
      hits <- index
      respond rest hits
    "packages" : rest -> do
      hits <- packages
      respond rest hits
    "types" : file : rest -> do
      hits <- types file
      respond rest hits
    _ -> error "invalid parameters"
