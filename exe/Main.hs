{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (isPrefixOf, stripPrefix)
import           DynFlags (DynFlags(..), FlagSpec(..), GhcLink(..),
                           HscTarget(..), unsafeGlobalDynFlags, xFlags,
                           xopt_set, xopt_unset)
import qualified GHC as GHC
import           GHC.Paths (libdir)
import           HscTypes (Target(..), TargetId(..), mgModSummaries)
import           Outputable (Outputable, showPpr)
import           RdrName (GlobalRdrElt(..), ImpDeclSpec(..), ImportSpec(..),
                          globalRdrEnvElts)
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
imports exts file = do
  dflags <- GHC.getSessionDynFlags
  let
    -- TODO is there a ghc utility to set lang extensions?
    getX = flip lookup $ (\f -> (flagSpecName f, flagSpecFlag f)) <$> xFlags
    update f (stripPrefix "-XNo" -> Just (getX -> Just ux)) = xopt_unset f ux
    update f (stripPrefix "-X" -> Just (getX -> Just ux)) = xopt_set f ux
    update f _ = f
    dflags' = foldl update dflags exts

  liftIO $ putStrLn $ showGhc $ extensions dflags'
  void $ GHC.setSessionDynFlags $ dflags' {
      hscTarget = HscNothing
    , ghcLink   = NoLink
    }

  let target = Target (TargetFile file Nothing) False Nothing
  GHC.setTargets [target]
  _ <- GHC.load GHC.LoadAllTargets

  graph <- GHC.getModuleGraph

  liftIO $ putStrLn $ showGhc $ mgModSummaries graph

  importsInScope . GHC.ms_mod_name . head . mgModSummaries $ graph

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- like modInfoTopLevelScope but with qualification information
--
-- WORKAROUND minf_rdr_env is not visible from ModuleInfo so we
-- need to do another parse / typecheck to get the tm_internals_
--
-- TODO send a PR to upstream to add the necessary feature
importsInScope :: GHC.GhcMonad m => GHC.ModuleName -> m [GlobalRdrElt]
importsInScope m = do
  modSum <- GHC.getModSummary m
  -- TODO don't parse beyond the import section
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
      minf_rdr_env = tcg_rdr_env tc_gbl_env
  pure $ globalRdrEnvElts minf_rdr_env

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
