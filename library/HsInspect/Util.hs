module HsInspect.Util where

import System.Directory (doesDirectoryExist, listDirectory)

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
