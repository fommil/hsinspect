{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Workarounds where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (delete, intercalate, isSuffixOf)
import           DriverPhases (HscSource(..), Phase(..))
import           DriverPipeline (preprocess)
import           DynFlags (parseDynamicFilePragma)
import           FastString
import qualified GHC as GHC
import           HeaderInfo (getOptions)
import           HscTypes (Target(..), TargetId(..))
import           HsImpExp (ImportDecl(..))
import           Lexer
import           Outputable (showPpr)
import           Parser (parseHeader)
import           RdrName (GlobalRdrEnv)
import           SrcLoc
import           StringBuffer
import           System.Directory (getModificationTime, removeFile)
import           TcRnTypes (tcg_rdr_env)

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
importsOnly :: GHC.GhcMonad m => [GHC.ModuleName] -> FilePath -> m (Maybe GHC.ModuleName, Target)
importsOnly homes file = do
  sess <- GHC.getSession
  (dflags, tmp) <- liftIO $ preprocess sess (file, Nothing)
  full <- liftIO $ hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let pragmas = getOptions dflags full file
      loc  = mkRealSrcLoc (mkFastString file) 1 1
      -- TODO elem is a potential perf problem
      allowed (L _ (ImportDecl{ideclName})) = notElem (unLoc ideclName) homes
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
      allowed (L _ (XImportDecl _)) = False
#endif
  (dflags', _, _) <- parseDynamicFilePragma dflags pragmas
  (modname, trimmed) <- case unP parseHeader (mkPState dflags' full loc) of
    POk _ (L _ hsmod) -> do
      let modname = unLoc <$> GHC.hsmodName hsmod
          extra =
            if modname == Nothing || modname == (Just $ GHC.mkModuleName "Main")
            then "\nmain = return ()" -- TODO check that return is imported
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
  pure $ (modname, Target (TargetFile file (Just $ Hsc HsSrcFile)) False (Just (trimmed, ts)))

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
minf_rdr_env' :: GHC.GhcMonad m => GHC.ModuleName -> m GlobalRdrEnv
minf_rdr_env' m = do
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
  pure $ tcg_rdr_env tc_gbl_env
