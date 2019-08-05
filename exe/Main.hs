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
-- TODO chop off everything below the import section
main :: IO ()
main = GHC.runGhc (Just libdir) $ do
  "imports" : file : [] <- liftIO $ getArgs
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags $ dflags {
      hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }

  let target = Target (TargetFile file Nothing) False Nothing
  GHC.setTargets [target]
  _ <- GHC.load GHC.LoadAllTargets

  graph <- GHC.getModuleGraph
  let m = GHC.ms_mod_name . head . mgModSummaries $ graph
  modSum <- GHC.getModSummary m

  -- TODO is parsing/typechecking redoing work?
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod

  let quals = describe =<< modInfoTopLevelScope' tmod
  liftIO $ putStrLn "("
  forM_ quals (liftIO . putStrLn . toSexp)
  liftIO $ putStrLn ")"

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- like modInfoTopLevelScope but with original qualification information
modInfoTopLevelScope' :: GHC.TypecheckedModule -> [GlobalRdrElt]
modInfoTopLevelScope' tmod =
  -- WORKAROUND minf_rdr_env is not visible from ModuleInfo
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
      minf_rdr_env = tcg_rdr_env tc_gbl_env
  in globalRdrEnvElts minf_rdr_env

describe :: GlobalRdrElt -> [Qualified]
describe GRE{gre_name, gre_imp} = describe' <$> gre_imp
  where
    describe' ImpSpec{is_decl=ImpDeclSpec{is_mod, is_as, is_qual}} =
      let fqn = showGhc is_mod ++ "." ++ showGhc gre_name
          lqn = if is_qual then Nothing else Just $
                  if is_mod /= is_as
                  then showGhc is_as ++ "." ++ showGhc gre_name
                  else showGhc gre_name
      in Qualified lqn fqn
      -- Note that `nameSrcLoc gre_name` is empty
      -- TODO what other information is available?
      -- TODO "and originally defined" / ppr_defn_site

-- TODO use Text or FastString
data Qualified = Qualified (Maybe String) String -- ^^ local name, qualified name
  deriving (Eq, Show)

-- TODO an Sexp package to avoid manual string manipulation
-- TODO find a sexp2json tool for non-Emacs users or
--      consider using the Json package since it is in ghc
toSexp :: Qualified -> String
toSexp (Qualified (Just lqn) fqn) =
  concat ["(:lqn ", show lqn, " ", show fqn, ")"]
toSexp (Qualified Nothing fqn) =
  concat ["(:fqn ", show fqn, ")"]
