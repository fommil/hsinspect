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
  "                             unqualified) names."

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
    let modules = GHC.mkModuleName <$> (filter (isUpper . head) ghcargs)
    GHC.setTargets $ (\m -> GHC.Target (GHC.TargetModule m) True Nothing) <$> modules
    case args of
      "imports" : file : rest -> do
        quals <- imports file
        let output = if (elem "--json" rest)
                     then encodeJson dflags' . JSArray $ (json <$> quals)
                     else S.encode quals
        liftIO . putStrLn $ output
      _ ->
        liftIO $ error "invalid parameters"

encodeJson :: GHC.DynFlags -> JsonDoc -> String
encodeJson dflags = show . flip runSDoc ctx . renderJSON
  where ctx = initSDocContext dflags $ defaultUserStyle dflags
