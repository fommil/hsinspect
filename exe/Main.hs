module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           DynFlags (DynFlags(..), GhcLink(..), HscTarget(..),
                           unsafeGlobalDynFlags)
import qualified GHC as GHC
import           GHC.Paths (libdir)
import           Outputable (Outputable, showPpr)
import           RdrName (GlobalRdrElt, globalRdrEnvElts)
import           TcRnTypes (tcg_rdr_env)

-- TODO input file
-- TODO user provided default language extensions
-- TODO infer default language extensions
main :: IO ()
main = GHC.runGhc (Just libdir) $ do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags $ dflags {
      hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }

  -- TODO don't repeat the filename
  t <- GHC.guessTarget "exe/Main.hs" Nothing
  GHC.setTargets [t]
  _ <- GHC.load GHC.LoadAllTargets

  modSum <- GHC.getModSummary $ GHC.mkModuleName "Main"

  -- TODO is parsing/typechecking redoing work?
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod

  let Just (_, imports, _, _) = GHC.tm_renamed_source tmod
  liftIO . putStrLn . showGhc $ imports

  -- TODO (local name, fully qualified name, (optional) normalised type sig)
  -- TODO s-expression output
  -- TODO find a good sexp2json tool for non-Emacs users
  liftIO . putStrLn . showGhc $ modInfoTopLevelScope' tmod

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- like modInfoTopLevelScope but with original qualification information
modInfoTopLevelScope' :: GHC.TypecheckedModule -> [GlobalRdrElt]
modInfoTopLevelScope' tmod =
  -- WORKAROUND minf_rdr_env is not visible from ModuleInfo
  let (tc_gbl_env, _) = GHC.tm_internals_ tmod
      minf_rdr_env = tcg_rdr_env tc_gbl_env
  in globalRdrEnvElts minf_rdr_env
