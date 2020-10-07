
module HsInspect.LSP.Util where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.List (intercalate)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import qualified System.Log.Logger as L
import qualified System.Process as P

shell :: String -> [String] -> Maybe FilePath -> Maybe String -> [(String, String)] -> ExceptT String IO String
shell command args cwd path env_extra = ExceptT $ do
  env <- getEnvironment
  let env' = maybe env (\p -> ("PATH", p) : env) path
      process = (P.proc command args) { P.env = Just (env_extra <> env')
                                      , P.cwd = cwd }
  liftIO $ L.debugM "haskell-lsp" $ "hsinspect-lsp:shell:" <> command <> " " <> intercalate " " args
  (code, stdout, stderr) <- P.readCreateProcessWithExitCode process ""
  case code of
    ExitFailure i -> pure . Left $
      concat [ "exit code: ", show i
             , "\n stdout: ", stdout
             , "\n stderr: ", stderr ]
    ExitSuccess -> pure $ Right stdout
