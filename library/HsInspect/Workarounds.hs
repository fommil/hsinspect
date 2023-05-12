{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Workarounds where

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Driver.Pipeline as Pipeline
import qualified GHC.Driver.Session as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Types.Target as GHC
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config as GHC
import qualified GHC.Driver.Ppr as GHC
#else
import qualified DriverPipeline as Pipeline
import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified HeaderInfo as GHC
import qualified HscTypes as GHC
import qualified Lexer as GHC
import qualified Outputable as GHC
import qualified Parser as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
import qualified GHC.Hs.ImpExp as GHC
#else
import qualified HsImpExp as GHC
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Types.Name.Occurrence as GHC
#elif MIN_VERSION_GLASGOW_HASKELL(8,8,2,0)
import qualified OccName as GHC
#else
import qualified TcRnTypes as GHC
#endif
import qualified GHC as GHC

#if MIN_VERSION_GLASGOW_HASKELL(8,8,2,0)
import Data.Maybe (fromJust, fromMaybe)
#endif
import Control.Monad
import Control.Monad.IO.Class
import Data.List (delete, intercalate, isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (getModificationTime, removeFile)

-- applies CPP rules to the input file and extracts the pragmas,
-- a more reliable alternative to GHC.hGetStringBuffer
mkCppState :: GHC.HscEnv -> FilePath -> IO (GHC.PState, [GHC.Located String])
mkCppState sess file = do
#if MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
  pp <- Pipeline.preprocess sess file Nothing Nothing
  let (dflags, tmp) = case pp of
        Left _ -> error $ "preprocessing failed " <> show file
        Right success -> success
#else
  (dflags, tmp) <- preprocess sess (file, Nothing)
#endif
  full <- GHC.hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let pragmas = GHC.getOptions dflags full file
      loc  = GHC.mkRealSrcLoc (GHC.mkFastString file) 1 1
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
      mkPState' = GHC.initParserState . GHC.initParserOpts
#else
      mkPState' = GHC.mkPState
#endif
  (dflags', _, _) <- GHC.parseDynamicFilePragma dflags pragmas
  pure $ (mkPState' dflags' full loc, pragmas)

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
parseHeader' :: GHC.GhcMonad m => FilePath -> m ([String], GHC.HsModule)
#else
parseHeader' :: GHC.GhcMonad m => FilePath -> m ([String], GHC.HsModule GHC.GhcPs)
#endif
parseHeader' file = do
  sess <- GHC.getSession
  (pstate, pragmas) <- liftIO $ mkCppState sess file
  case GHC.unP GHC.parseHeader pstate of
    GHC.POk _ (GHC.L _ hsmod) -> pure (GHC.unLoc <$> pragmas, hsmod)
    _ -> error $ "parseHeader failed for " <> file

importsOnly :: GHC.GhcMonad m => Set GHC.ModuleName -> FilePath -> m (Maybe GHC.ModuleName, GHC.Target)
importsOnly homes file = do
  dflags <- GHC.getSessionDynFlags
  (pragmas, hsmod) <- parseHeader' file
  let allowed :: GHC.GenLocated l (GHC.ImportDecl GHC.GhcPs) -> Bool
      allowed (GHC.L _ (GHC.ImportDecl{GHC.ideclName})) = Set.notMember (GHC.unLoc ideclName) homes
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
#elif MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
      allowed (GHC.L _ (GHC.XImportDecl _)) = False
#endif
      modname = GHC.unLoc <$> GHC.hsmodName hsmod
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
        GHC.showPpr dflags (hsmod { GHC.hsmodExports = Nothing
                               , GHC.hsmodImports = imps }) <>
        extra
      trimmed = GHC.stringToStringBuffer contents

  ts <- liftIO $ getModificationTime file
  -- since 0f9ec9d1ff can't use Phase
  pure $ (modname, GHC.Target (GHC.TargetFile file Nothing) False (Just (trimmed, ts)))

parseModuleName' :: GHC.GhcMonad m => FilePath -> m (Maybe GHC.ModuleName)
parseModuleName' file = do
  (_, hsmod) <- parseHeader' file
  pure $ GHC.unLoc <$> GHC.hsmodName hsmod

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
minf_rdr_env' :: GHC.GhcMonad m => GHC.ModuleName -> m GHC.GlobalRdrEnv
minf_rdr_env' m = do
#if MIN_VERSION_GLASGOW_HASKELL(8,8,2,0)
  mo <- GHC.findModule m Nothing
  (fromJust -> mi) <- GHC.getModuleInfo mo
  pure . fromMaybe GHC.emptyOccEnv $ GHC.modInfoRdrEnv mi
#else
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
  pure $ tcg_rdr_env tc_gbl_env
#endif
