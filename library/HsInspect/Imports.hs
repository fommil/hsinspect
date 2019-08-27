{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Imports where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (delete, intercalate, isSuffixOf)
import           DriverPhases (HscSource(..), Phase(..))
import           DriverPipeline (preprocess)
import           DynFlags (parseDynamicFilePragma, unsafeGlobalDynFlags)
import           FastString
import qualified GHC as GHC
import           HeaderInfo (getOptions)
import           HscTypes (Target(..), TargetId(..))
import           HsInspect.Sexp
import           Json
import           Lexer
import           Outputable (Outputable, showPpr)
import           Parser (parseHeader)
import           RdrName (GlobalRdrElt(..), GlobalRdrEnv, ImpDeclSpec(..),
                          ImportSpec(..), globalRdrEnvElts)
import           SrcLoc
import           StringBuffer
import           System.Directory (getModificationTime, removeFile)
import           TcRnTypes (tcg_rdr_env)

imports :: GHC.GhcMonad m => FilePath -> m [Qualified]
imports file = do
  gres <- imports' file
  pure $ describe =<< gres

imports' :: GHC.GhcMonad m => FilePath -> m [GlobalRdrElt]
imports' file = do
  (m, target) <- workaroundGhc file

  GHC.removeTarget $ TargetModule m
  GHC.addTarget target

  _ <- GHC.load $ GHC.LoadUpTo m

  rdr_env <- minf_rdr_env' m
  pure $ globalRdrEnvElts rdr_env

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- TODO CPP should use the trivial impl in ghc 8.8

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
workaroundGhc :: GHC.GhcMonad m => FilePath -> m (GHC.ModuleName, Target)
workaroundGhc file = do
  sess <- GHC.getSession
  (dflags, tmp) <- liftIO $ preprocess sess (file, Nothing)
  full <- liftIO $ hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let pragmas = getOptions dflags full file
      loc  = mkRealSrcLoc (mkFastString file) 1 1
  (dflags', _, _) <- parseDynamicFilePragma dflags pragmas
  (modname, trimmed) <- case unP parseHeader (mkPState dflags' full loc) of
    POk _ (L _ hsmod@(GHC.hsmodName -> (Just (L _ modname)))) -> do
      let extra =
            if modname == (GHC.mkModuleName "Main")
            then "\nmain = return ()" -- TODO check that return is imported
            else ""
          -- WORKAROUND https://gitlab.haskell.org/ghc/ghc/issues/17066
          --            cannot use CPP in combination with targetContents
          pragmas' = delete "-XCPP" (unLoc <$> pragmas)
          contents =
            "{-# OPTIONS_GHC " <> (intercalate " " pragmas') <> " #-}\n" <>
            showPpr dflags' (hsmod { GHC.hsmodExports = Nothing }) <>
            extra
      pure (modname, stringToStringBuffer contents)
    _ -> error "parseHeader failed"

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

describe :: GlobalRdrElt -> [Qualified]
describe GRE{gre_name, gre_imp} = describe' <$> gre_imp
  where
    describe' ImpSpec{is_decl=ImpDeclSpec{is_mod, is_as, is_qual}} =
      let ln  = if is_qual
                then Nothing
                else Just $ showGhc gre_name
          lqn = if is_mod == is_as
                then Nothing
                else Just $ showGhc is_as ++ "." ++ showGhc gre_name
          fqn = showGhc is_mod ++ "." ++ showGhc gre_name
      in Qualified ln lqn fqn
      -- Note that `nameSrcLoc gre_name` is empty
      -- TODO what other information is available?
      -- TODO "and originally defined" / ppr_defn_site

data Qualified = Qualified
                   (Maybe String) -- ^^ local name
                   (Maybe String) -- ^^ locally qualifed name
                   String         -- ^^ fully qualified name
  deriving (Eq, Show)

instance ToSexp Qualified where
  toSexp (Qualified ln lqn fqn) =
    alist [ ("local", toSexp ln)
          , ("qual", toSexp lqn)
          , ("full", toSexp fqn)]

instance ToJson Qualified where
  json (Qualified ln lqn fqn) =
    JSObject [ ("local", json' ln)
             , ("qual" , json' lqn)
             , ("full" , JSString fqn)]
    where json' Nothing = JSNull
          json' (Just a) = JSString a
