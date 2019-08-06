{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           DynFlags (DynFlags(..), GhcLink(..), HscTarget(..),
                           unsafeGlobalDynFlags)
import qualified GHC as GHC
import           GHC.Paths (libdir)
import           HscTypes (Target(..), TargetId(..), mgModSummaries)
import           Outputable (Outputable, showPpr)
import           RdrName (GlobalRdrElt(..), ImpDeclSpec(..), ImportSpec(..),
                          globalRdrEnvElts)
import           System.Environment (getArgs)
import           TcRnTypes (tcg_rdr_env)

-- TODO user provided default language extensions
-- TODO infer default language extensions
main :: IO ()
main = GHC.runGhc (Just libdir) $ do
  args <- liftIO $ getArgs
  case args of
    "imports" : file : _ -> do
      gres <- imports file
      liftIO $ putStrLn "("
      forM_ (describe =<< gres) (liftIO . putStrLn . toSexp)
      liftIO $ putStrLn ")"
    _ ->
      liftIO $ error "invalid parameters"

imports :: GHC.GhcMonad m => FilePath -> m [GlobalRdrElt]
imports file = do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags $ dflags {
      hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }

  let target = Target (TargetFile file Nothing) False Nothing
  GHC.setTargets [target]
  _ <- GHC.load GHC.LoadAllTargets

  graph <- GHC.getModuleGraph
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
-- TODO an Sexp package to avoid manual string manipulation
-- TODO find a sexp2json tool for non-Emacs users or
--      consider using the Json package since it is in ghc
toSexp :: Qualified -> String
toSexp (Qualified ln lqn fqn) =
  concat $ ["("] ++ m2s ln ++ m2s lqn ++ [show fqn] ++ [")"]
  where
    m2s (Just s) = [show s, " "]
    m2s Nothing = []
