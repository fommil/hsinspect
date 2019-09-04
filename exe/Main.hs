{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (isUpper)
import           DynFlags (parseDynamicFlagsCmdLine)
import qualified GHC as GHC
import           GHC.Paths (libdir)
import           HsInspect.Imports
import           HsInspect.Modules
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
  "  search  /path/to/file.hs QUERY - Hoogle query within the file's context.\n "

-- Possible backends:
--
-- https://github.com/mpickering/hie-bios
-- http://hackage.haskell.org/package/cabal-helper
main :: IO ()
main = do
  (break ("--" ==) -> (args, flags)) <- getArgs
  when (elem "--help" args) $
    (putStrLn help) >> exitWith ExitSuccess
  when (elem "--version" args) $
    (putStrLn version) >> exitWith ExitSuccess
  GHC.runGhc (Just libdir) $ do
    dflags <- GHC.getSessionDynFlags
    (dflags', (GHC.unLoc <$>) -> ghcargs, _) <- liftIO $ parseDynamicFlagsCmdLine dflags (GHC.noLoc <$> tail flags)
    void $ GHC.setSessionDynFlags dflags'
           { GHC.hscTarget = GHC.HscNothing
           , GHC.ghcLink   = GHC.NoLink
           }
    let homeModules = (filter (isUpper . head) ghcargs)
    GHC.setTargets $
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
      "search" : query : rest -> do
        hits <- search query
        respond rest hits
      _ ->
        liftIO $ error "invalid parameters"

encodeJson :: ToJson a => GHC.DynFlags -> [a] -> String
encodeJson dflags as = show . flip runSDoc ctx . renderJSON $ JSArray (json <$> as)
  where ctx = initSDocContext dflags $ defaultUserStyle dflags
