{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (delete, intercalate, isPrefixOf, isSuffixOf,
                            stripPrefix)
import           DriverPhases (HscSource(..), Phase(..))
import           DriverPipeline (preprocess)
import           DynFlags (DynFlags(..), FlagSpec(..), GhcLink(..),
                           HscTarget(..), unsafeGlobalDynFlags, xFlags,
                           xopt_set, xopt_unset)
import           FastString
import qualified GHC as GHC
import           GHC.LanguageExtensions (Extension)
import           GHC.Paths (libdir)
import           HeaderInfo (getOptions)
import           HscTypes (Target(..), TargetId(..), mgModSummaries)
import           Lexer
import           Outputable (Outputable, showPpr)
import           Parser (parseHeader)
import           RdrName (GlobalRdrElt(..), GlobalRdrEnv, ImpDeclSpec(..),
                          ImportSpec(..), globalRdrEnvElts)
import           SrcLoc
import           StringBuffer
import           System.Directory (getModificationTime, removeFile)
import           System.Environment (getArgs)
import           TcRnTypes (tcg_rdr_env)

-- TODO infer default language extensions
-- TODO infer language version
--
-- Possible backends:
--
-- https://github.com/mpickering/hie-bios
-- http://hackage.haskell.org/package/cabal-helper
main :: IO ()
main = GHC.runGhc (Just libdir) $ do
  args <- liftIO $ getArgs
  case args of
    -- TODO --version
    -- TODO --help
    "imports" : file : user -> do
      let exts = filter ("-X" `isPrefixOf`) user
          _json = any ("-json" ==) user
      gres <- imports exts file
      let descs = describe =<< gres
      liftIO $ putStrLn "("
      forM_ descs (liftIO . putStrLn . toSexp)
      liftIO $ putStrLn ")"
    _ ->
      liftIO $ error "invalid parameters"

imports :: GHC.GhcMonad m => [String] -> FilePath -> m [GlobalRdrElt]
imports user_exts file = do
  dflags <- GHC.getSessionDynFlags
  let
    dflags' = (foldl update dflags user_exts) {
        hscTarget = HscNothing
      , ghcLink   = NoLink
      }
  void . GHC.setSessionDynFlags $ dflags'

  target <- workaroundGhc file
  GHC.setTargets [target]
  _ <- GHC.load GHC.LoadAllTargets

  graph <- GHC.getModuleGraph
  rdr_env <- minf_rdr_env' . GHC.ms_mod_name . head . mgModSummaries $ graph
  pure $ globalRdrEnvElts rdr_env

-- TODO is there a ghc utility to update DynFlags from [String]?
--             (_, dflags') = runCmdLine (runEwM setFlags) dflags
getX :: String -> Maybe Extension
getX = flip lookup $ (\f -> (flagSpecName f, flagSpecFlag f)) <$> xFlags

update :: DynFlags -> String -> DynFlags
update f (stripPrefix "-XNo" -> Just (getX -> Just ux)) = xopt_unset f ux
update f (stripPrefix "-X" -> Just (getX -> Just ux)) = xopt_set f ux
update f _ = f

println :: (Outputable a, GHC.GhcMonad m) => a -> m ()
println = liftIO . putStrLn . showGhc

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
workaroundGhc :: GHC.GhcMonad m => FilePath -> m Target
workaroundGhc file = do
  sess <- GHC.getSession
  (dflags, tmp) <- liftIO $ preprocess sess (file, Nothing)
  full <- liftIO $ hGetStringBuffer tmp
  when (".hscpp" `isSuffixOf` tmp) $
    liftIO . removeFile $ tmp
  let
    file_exts = unLoc <$> getOptions dflags full file
    dflags' = foldl update dflags file_exts
    loc  = mkRealSrcLoc (mkFastString file) 1 1
  trimmed <- case unP parseHeader (mkPState dflags' full loc) of
    POk _ (L _ hsmod) -> do
      let extra =
            if (unLoc <$> GHC.hsmodName hsmod) == (Just $ GHC.mkModuleName "Main")
            then "\nmain = return ()" -- TODO check that return is imported
            else ""
          -- WORKAROUND https://gitlab.haskell.org/ghc/ghc/issues/17066
          --            cannot use CPP in combination with targetContents
          filtered_exts = delete "-XCPP" file_exts
          contents =
            "{-# OPTIONS_GHC " <> (intercalate " " filtered_exts) <> " #-}\n" <>
            showPpr dflags' (hsmod { GHC.hsmodExports = Nothing }) <>
            extra
      -- liftIO . putStrLn $ contents
      pure . stringToStringBuffer $ contents
    _ -> error "parseHeader failed"

  ts <- liftIO $ getModificationTime file
  pure $ Target (TargetFile file (Just $ Hsc HsSrcFile)) False (Just (trimmed, ts))

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

-- TODO use Text or FastString
data Qualified = Qualified
                   (Maybe String) -- ^^ local name
                   (Maybe String) -- ^^ locally qualifed name
                   String         -- ^^ fully qualified name
  deriving (Eq, Show)

-- TODO alist or :keyword instead of unlabelled list
-- TODO Sexp module
-- TODO Json module from ghc
toSexp :: Qualified -> String
toSexp (Qualified ln lqn fqn) =
  concat $ ["("] ++ m2s ln ++ m2s lqn ++ [show fqn] ++ [")"]
  where
    m2s (Just s) = [show s, " "]
    m2s Nothing = []
