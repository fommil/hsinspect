{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (isPrefixOf, stripPrefix)
import           Data.Time.Clock (UTCTime)
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
import           System.Directory (getModificationTime)
import           System.Environment (getArgs)
import           TcRnTypes (tcg_rdr_env)

-- TODO tests for modules that depend on other modules in the same package
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

  hack <- workaroundGhc file
  let target = Target (TargetFile file Nothing) False hack
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

-- FIXME support preprocessing of source files (needed for our tests)
-- hsinspect: buffer needs preprocesing; interactive check disabled

-- WORKAROUND https://gitlab.haskell.org/ghc/ghc/merge_requests/1541
workaroundGhc :: GHC.GhcMonad m => FilePath -> m (Maybe (StringBuffer, UTCTime))
workaroundGhc file = do
  dflags <- GHC.getSessionDynFlags
  full <- liftIO $ hGetStringBuffer file
  let
    file_exts = unLoc <$> getOptions dflags full file
    dflags' = foldl update dflags file_exts
    loc  = mkRealSrcLoc (mkFastString file) 1 1
  trimmed <- case unP parseHeader (mkPState dflags' full loc) of
    POk _ (L _ hsmod) ->
      -- TODO if the module is called Main then append `main = return ()`
      pure . stringToStringBuffer $ showPpr dflags' hsmod
    _ -> error "parseHeader failed"

  -- TODO don't update the global dflags, instead render them into `trimmed'
  void . GHC.setSessionDynFlags $ dflags'

  ts <- liftIO $ getModificationTime file
  pure $ Just (trimmed, ts)

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
