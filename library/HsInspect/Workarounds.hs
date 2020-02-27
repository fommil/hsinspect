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

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
importsOnly :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m (Maybe GHC.ModuleName, Target)
importsOnly homes file = do
  sess <- GHC.getSession
  -- NOTE: the behaviour of preprocess changed in 8.8.1 and it no longer reads
  -- and sets LANGUAGE pragamas from in header of the file. Since this function
  -- is no longer needed in 8.8.2 we don't bother fixing this.
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
      allowed (L _ (ImportDecl{ideclName})) = Set.notMember (unLoc ideclName) homes
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
      allowed (L _ (XImportDecl _)) = False
#endif
  (dflags', _, _) <- parseDynamicFilePragma dflags pragmas
  (modname, trimmed) <- case unP parseHeader (mkPState dflags' full loc) of
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
            showPpr dflags' (hsmod { GHC.hsmodExports = Nothing
                                   , GHC.hsmodImports = imps }) <>
            extra
      pure (modname, stringToStringBuffer contents)
    _ -> error  $ "parseHeader failed for " <> file

  ts <- liftIO $ getModificationTime file
  -- HsSrcFile here broken on 8.8.1
  pure $ (modname, Target (TargetFile file Nothing) False (Just (trimmed, ts)))

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


