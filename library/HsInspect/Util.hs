{-# LANGUAGE NamedFieldPuns #-}

module HsInspect.Util where

import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC as GHC
import System.Directory (doesDirectoryExist, listDirectory)

getTargetModules :: GHC.GhcMonad m => m (Set GHC.ModuleName)
getTargetModules = do
  args <- GHC.getTargets
  pure . Set.fromList . catMaybes $ getModule <$> args
  where
    getModule :: GHC.Target -> Maybe GHC.ModuleName
    getModule GHC.Target{GHC.targetId} = case targetId of
      GHC.TargetModule m -> Just m
      GHC.TargetFile _ _ -> Nothing

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
