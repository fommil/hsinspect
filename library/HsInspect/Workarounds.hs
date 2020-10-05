{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Workarounds where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (delete, intercalate, isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import DriverPipeline (preprocess)
import DynFlags (parseDynamicFilePragma)
import FastString
import qualified GHC as GHC
import HeaderInfo (getOptions)
import HscTypes (HscEnv, Target(..), TargetId(..))
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
import GHC.Hs.ImpExp (ImportDecl(..))
#else
import HsImpExp (ImportDecl(..))
#endif
import Lexer
import Outputable (showPpr)
import Parser (parseHeader)
import RdrName (GlobalRdrEnv)
import SrcLoc
import StringBuffer
import System.Directory (getModificationTime, removeFile)

#if MIN_VERSION_GLASGOW_HASKELL(8,8,2,0)
import Data.Maybe (fromJust, fromMaybe)
import OccName (emptyOccEnv)
#else
import TcRnTypes (tcg_rdr_env)
#endif

-- applies CPP rules to the input file and extracts the pragmas,
-- a more portable alternative to GHC.hGetStringBuffer
mkCppState :: HscEnv -> FilePath -> IO (PState, [Located String])
mkCppState sess file = do
#if MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
  pp <- preprocess sess file Nothing Nothing
  let (dflags, tmp) = case pp of
        Left _ -> error $ "preprocessing failed " <> show file
        Right success -> success
#else
  (dflags, tmp) <- preprocess sess (file, Nothing)
#endif
  full <- hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let pragmas = getOptions dflags full file
      loc  = mkRealSrcLoc (mkFastString file) 1 1
  (dflags', _, _) <- parseDynamicFilePragma dflags pragmas
  pure $ (mkPState dflags' full loc, pragmas)

parseHeader' :: GHC.GhcMonad m => FilePath -> m ([String], GHC.HsModule GHC.GhcPs)
parseHeader' file = do
  sess <- GHC.getSession
  (pstate, pragmas) <- liftIO $ mkCppState sess file
  case unP parseHeader pstate of
    POk _ (L _ hsmod) -> pure (unLoc <$> pragmas, hsmod)
    _ -> error $ "parseHeader failed for " <> file

importsOnly :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m (Maybe GHC.ModuleName, Target)
importsOnly homes file = do
  dflags <- GHC.getSessionDynFlags
  (pragmas, hsmod) <- parseHeader' file
  let allowed (L _ (ImportDecl{ideclName})) = Set.notMember (unLoc ideclName) homes
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
      allowed (L _ (XImportDecl _)) = False
#endif
      modname = unLoc <$> GHC.hsmodName hsmod
      extra =
        if modname == Nothing || modname == (Just $ GHC.mkModuleName "Main")
        then "\nmain = return ()"
        else ""
      imps = filter allowed $ GHC.hsmodImports hsmod
      -- WORKAROUND https://gitlab.haskell.org/ghc/ghc/issues/17066
      --            cannot use CPP in combination with targetContents
      pragmas' = delete "-XCPP" pragmas
      contents =
        "{-# OPTIONS_GHC " <> (intercalate " " pragmas') <> " #-}\n" <>
        showPpr dflags (hsmod { GHC.hsmodExports = Nothing
                               , GHC.hsmodImports = imps }) <>
        extra
      trimmed = stringToStringBuffer contents

  ts <- liftIO $ getModificationTime file
  -- since 0f9ec9d1ff can't use Phase
  pure $ (modname, Target (TargetFile file Nothing) False (Just (trimmed, ts)))

parseModuleName' :: GHC.GhcMonad m => FilePath -> m (Maybe GHC.ModuleName)
parseModuleName' file = do
  (_, hsmod) <- parseHeader' file
  pure $ unLoc <$> GHC.hsmodName hsmod

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
minf_rdr_env' :: GHC.GhcMonad m => GHC.ModuleName -> m GlobalRdrEnv
minf_rdr_env' m = do
#if MIN_VERSION_GLASGOW_HASKELL(8,8,2,0)
  mo <- GHC.findModule m Nothing
  (fromJust -> mi) <- GHC.getModuleInfo mo
  pure . fromMaybe emptyOccEnv $ GHC.modInfoRdrEnv mi
#else
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
  pure $ tcg_rdr_env tc_gbl_env
#endif


