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
import HscTypes (Target(..), TargetId(..))
import HsImpExp (ImportDecl(..))
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

parseHeader' :: GHC.GhcMonad m => FilePath -> m ([Located String], ParseResult (Located (GHC.HsModule GHC.GhcPs)))
parseHeader' file = do
  sess <- GHC.getSession
#if MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
  pp <- liftIO $ preprocess sess file Nothing Nothing
  let (dflags, tmp) = case pp of
        Left _ -> error $ "preprocessing failed " <> show file
        Right success -> success
#else
  (dflags, tmp) <- liftIO $ preprocess sess (file, Nothing)
#endif
  full <- liftIO $ hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let pragmas = getOptions dflags full file
      loc  = mkRealSrcLoc (mkFastString file) 1 1
  (dflags', _, _) <- parseDynamicFilePragma dflags pragmas
  let header = unP parseHeader (mkPState dflags' full loc)
  pure (pragmas, header)

importsOnly :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m (Maybe GHC.ModuleName, Target)
importsOnly homes file = do
  dflags <- GHC.getSessionDynFlags
  (pragmas, header) <- parseHeader' file
  let allowed (L _ (ImportDecl{ideclName})) = Set.notMember (unLoc ideclName) homes
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
      allowed (L _ (XImportDecl _)) = False
#endif
  (modname, trimmed) <- case header of
    POk _ (L _ hsmod) -> do
      let modname = unLoc <$> GHC.hsmodName hsmod
          extra =
            if modname == Nothing || modname == (Just $ GHC.mkModuleName "Main")
            then "\nmain = return ()"
            else ""
          imps = filter allowed $ GHC.hsmodImports hsmod
          -- WORKAROUND https://gitlab.haskell.org/ghc/ghc/issues/17066
          --            cannot use CPP in combination with targetContents
          pragmas' = delete "-XCPP" (unLoc <$> pragmas)
          contents =
            "{-# OPTIONS_GHC " <> (intercalate " " pragmas') <> " #-}\n" <>
            showPpr dflags (hsmod { GHC.hsmodExports = Nothing
                                   , GHC.hsmodImports = imps }) <>
            extra
      pure (modname, stringToStringBuffer contents)
    _ -> error  $ "parseHeader failed for " <> file

  ts <- liftIO $ getModificationTime file
  -- since 0f9ec9d1ff can't use Phase
  pure $ (modname, Target (TargetFile file Nothing) False (Just (trimmed, ts)))

parseModuleName' :: GHC.GhcMonad m => FilePath -> m (Maybe GHC.ModuleName)
parseModuleName' file = do
  (_, headers) <- parseHeader' file
  case headers of
    POk _ (L _ hsmod) -> pure $ unLoc <$> GHC.hsmodName hsmod
    _ -> error  $ "parseHeader failed for " <> file

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


