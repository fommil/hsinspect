
module HsInspect.LSP.Util where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..))
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (readProcessWithExitCode)

type ZIO = ExceptT String IO

-- the first parent directory where a file or directory name matches the predicate
locateDominating :: (String -> Bool) -> FilePath -> ZIO FilePath
locateDominating p dir = do
  files <- lift $ listDirectory dir
  let parent = takeDirectory dir
  if any p $ takeFileName <$> files
  then pure dir
  else if parent == dir
       then ExceptT . pure . Left $ "locateDominating"
       else locateDominating p parent

shell :: String -> [String] -> ZIO String
shell command args = ExceptT $ do
  (code, stdout, stderr) <- readProcessWithExitCode command args ""
  case code of
    ExitFailure i -> pure . Left $
      concat [ "exit code: ", show i
             , "\n stdout: ", stdout
             , "\n stderr: ", stderr ]
    ExitSuccess -> pure $ Right stdout
