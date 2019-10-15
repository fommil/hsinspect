{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (isUpper)
import           Data.List (isPrefixOf)
import           DynFlags (parseDynamicFlagsCmdLine, updOptLevel)
import qualified GHC as GHC
import           GHC.Paths (libdir)
import           HsInspect.Imports
import           HsInspect.Modules
import           HsInspect.Packages
import           HsInspect.Search
import           HsInspect.Sexp as S
import           Json
import           Outputable (defaultUserStyle, initSDocContext, runSDoc)
import           System.Environment (getArgs)
import           System.Exit

version :: String
#ifdef CURRENT_PACKAGE_VERSION
version = CURRENT_PACKAGE_VERSION
#else
version = "unknown"
#endif

help :: String
help =
  "hsinspect command ARGS [--json|help|version] -- [ghcflags]\n\n" ++
  "  `command ARGS' can be:\n\n" ++
  "  imports /path/to/file.hs - list the qualified imports for the file\n" ++
  "                             along with their locally qualified (and\n" ++
  "                             unqualified) names.\n" ++
  "  modules /path/to/file.hs - list all modules that could be imported by file.\n" ++
  "  packages /path/to/dir    - list all packages that are imported by this dir.\n" ++
  "  search  /path/to/file.hs QUERY - Hoogle query within the file's context.\n "

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
  -- TODO require a -B flag instead of the ghc-paths dependency
  GHC.runGhc (Just libdir) $ do
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
    let respond rest as = liftIO . putStrLn $
          if (elem "--json" rest)
          then encodeJson dflags' as
          else S.encode as
    case args of
      "imports" : file : rest -> do
        quals <- imports file
        respond rest quals
      "modules" : rest -> do
        hits <- modules homeModules
        respond rest hits
      "packages" : dir : rest -> do
        hits <- packages dir
        respond rest hits
      "search" : query : rest -> do
        hits <- search query
        respond rest hits
      _ ->
        liftIO $ error "invalid parameters"

-- TODO let each component remove things that interfere
filterFlags :: [String] -> [String]
filterFlags ("--" : rest) = filter allow rest
  where allow flag = "-Wno" `isPrefixOf` flag || not ("-W" `isPrefixOf` flag)
filterFlags _ = []

encodeJson :: ToJson a => GHC.DynFlags -> a -> String
encodeJson dflags as = show . flip runSDoc ctx . renderJSON . json $ as
  where ctx = initSDocContext dflags $ defaultUserStyle dflags

instance ToJson a => ToJson [a] where
  json as = JSArray $ json <$> as
