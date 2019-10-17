{-# LANGUAGE CPP #-}

-- | A ghc plugin that creates `.ghc.flags` files (and `.ghc.version`) populated
--   with the flags that were last used to invoke ghc for some modules, for
--   consumption by tools that need to know the build parameters.
--
--   https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins
module GhcFlags.Plugin
  ( plugin,
  )
where

import qualified Config as GHC
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (traverse_)
import           Data.List (stripPrefix)
import qualified GHC
import qualified GhcPlugins as GHC
import           System.Directory (doesFileExist)
import           System.Environment
import           System.IO.Error (catchIOError)

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.installCoreToDos = install
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
    , GHC.pluginRecompile = GHC.purePlugin
#endif
    }

install :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
install _ core = do
  dflags <- GHC.getDynFlags
  args <- liftIO $ getArgs

  -- downstream tools shouldn't use this plugin, or all hell will break loose
  let ghcFlags = unwords $ replace ["-fplugin", "GhcFlags.Plugin"] [] args

      -- TODO this currently only supports ghc being called with directories and
      -- home modules, we should also support calling with explicit file names.
      paths = GHC.importPaths dflags
      writeGhcFlags path = writeDifferent (path <> "/.ghc.flags") ghcFlags
      enable = case GHC.hscTarget dflags of
        GHC.HscInterpreted -> False
        GHC.HscNothing -> False
        _ -> True

  when enable $ liftIO $ do
    traverse_ writeGhcFlags paths
    writeDifferent ".ghc.version" GHC.cProjectVersion

  pure core

-- only writes out the file when it will result in changes, and silently fails
-- on exceptions because the plugin should never interrupt normal ghc work.
writeDifferent :: FilePath -> String -> IO ()
writeDifferent file content =
  ignoreIOExceptions
    $ whenM isDifferent (writeFile file content)
  where
    isDifferent =
      ifM (doesFileExist file) ((content /=) <$> readFile file) (pure True)

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
