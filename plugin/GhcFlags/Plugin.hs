{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- | A ghc plugin that creates `.ghc.flags` files populated with the flags that
--   were last used to invoke ghc for some modules, for consumption by tools
--   that need to know the build parameters.
--
--   https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins
module GhcFlags.Plugin
  ( plugin,
  )
where

import Control.Exception (finally, onException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (stripPrefix)
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, utctDayTime)
import qualified GHC
import qualified GhcPlugins as GHC
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Environment
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.installCoreToDos = install
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
    , GHC.pluginRecompile = GHC.purePlugin
#endif
    }

-- Only run "write" once per process, unfortunately there is no other way to
-- inject an IORef except to use Unsafe.
lock :: IORef Bool
lock = unsafePerformIO $ newIORef False

install :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
install _ core = do
  written <- liftIO $ atomicModifyIORef' lock (True,)
  unless written write
  pure core

-- TODO support incremental compilation
--
-- This only supports ghc being called with directories and home modules. That
-- means we don't support incremental compilation where ghc is called with
-- explicit filenames and dependencies. There are cases where the .ghc.flags may
-- get out of date, e.g. adding / removing home modules. To handle those cases
-- we need to detect them and write to a different (per-file) .ghc.flags
-- location. Text editors need to be aware of both kinds of ghc flags files and
-- use the most recent.
write :: (MonadIO m, GHC.HasDynFlags m) => m ()
write = do
  dflags <- GHC.getDynFlags
  args <- liftIO $ getArgs

  -- downstream tools shouldn't use this plugin, or all hell will break loose
  let ghcFlags = unwords $ replace ["-fplugin", "GhcFlags.Plugin"] [] args
      paths = GHC.importPaths dflags
      writeGhcFlags path = writeDifferent (path <> "/.ghc.flags") ghcFlags
      enable = case GHC.hscTarget dflags of
        GHC.HscInterpreted -> False
        GHC.HscNothing -> False
        _ -> True

  when enable $ liftIO $ do
    traverse_ writeGhcFlags paths

-- Only writes out the file when it will result in changes, and silently fails
-- on exceptions because the plugin should never interrupt normal ghc work. The
-- write is done atomically (via a temp file) otherwise it is prone to a lot of
-- crosstalk and then all writers fail to update the file.
writeDifferent :: FilePath -> String -> IO ()
writeDifferent file content =
  ignoreIOExceptions . whenM isDifferent $ do
    time <- getCurrentTime
    let hash = diffTimeToPicoseconds . utctDayTime $ time
        tmp = file <> "." <> show hash
    finally
      (writeFile tmp content >> renameFile tmp file)
      (removeFile tmp)
  where
    isDifferent =
      onException
        (ifM (doesFileExist file) ((content /=) <$> readFile file) (pure True))
        (pure True)

-- from Data.List.Extra
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs' <- stripPrefix from xs = to ++ replace from to xs'
replace from to (x : xs) = x : replace from to xs
replace _ _ [] = []

-- from Control.Monad.Extra
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (return ())

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- from System.Directory.Internal
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catchIOError` (\_ -> pure ())
