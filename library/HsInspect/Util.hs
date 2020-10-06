{-# LANGUAGE NamedFieldPuns #-}

module HsInspect.Util where

import Control.Monad.IO.Class
import Data.List (find, isSuffixOf, nub)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import DynFlags (unsafeGlobalDynFlags)
import qualified GHC as GHC
import Outputable (Outputable, showPpr)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName, (</>))

homeSources :: GHC.GhcMonad m => m [FilePath]
homeSources = do
  dflags <- GHC.getSessionDynFlags
  paths <- liftIO . traverse makeAbsolute $ GHC.importPaths dflags
  let infer dir = liftIO $ walkSuffix ".hs" dir
  nub . concat <$> traverse infer paths

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

getTargetModules :: GHC.GhcMonad m => m (Set GHC.ModuleName)
getTargetModules = do
  args <- GHC.getTargets
  pure . Set.fromList . catMaybes $ getModule <$> args
  where
    getModule :: GHC.Target -> Maybe GHC.ModuleName
    getModule GHC.Target{GHC.targetId} = case targetId of
      GHC.TargetModule m -> Just m
      GHC.TargetFile _ _ -> Nothing

-- returns the first file that matches the predicate
locateDominating :: (String -> Bool) -> FilePath -> IO (Maybe FilePath)
locateDominating p dir = do
  files <- listDirectory dir
  let parent = takeDirectory dir
  case find p $ takeFileName <$> files of
    Just file -> pure . Just $ dir </> file
    Nothing ->
      if parent == dir
       then pure Nothing
       else locateDominating p parent

-- the first parent directory where a file or directory name matches the predicate
locateDominatingDir :: (String -> Bool) -> FilePath -> IO (Maybe FilePath)
locateDominatingDir p dir = do
  file' <- locateDominating p dir
  pure $ takeDirectory <$> file'

walkSuffix :: String -> FilePath -> IO [FilePath]
walkSuffix suffix dir = filter (suffix `isSuffixOf`) <$> walk dir

walk :: FilePath -> IO [FilePath]
walk dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      fs <- listDirectory dir
      let base = dir <> "/"
          qfs = (base <>) <$> fs
      concatMapM walk qfs
    else pure [dir]

-- from extra
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
  where
    f x xs = do
      x' <- op x
      if null x'
        then xs
        else do
          xs' <- xs
          pure $ x' ++ xs'

-- from extra
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split f (x : xs) | f x = [] : split f xs
                 | y : ys <- split f xs = (x : y) : ys
                 | otherwise = [[]] -- never happens
