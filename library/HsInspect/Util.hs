{-# LANGUAGE NamedFieldPuns #-}

module HsInspect.Util where

import Control.Monad.IO.Class
import Data.List (find, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import DynFlags (unsafeGlobalDynFlags)
import qualified GHC as GHC
import Outputable (Outputable, showPpr)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (dropExtension, isPathSeparator)

inferHomeModules :: GHC.GhcMonad m => m [GHC.ModuleName]
inferHomeModules = do
  dflags <- GHC.getSessionDynFlags
  paths <- liftIO . traverse makeAbsolute $ GHC.importPaths dflags
  let infer dir = (inferModuleNameFor dir <$>) <$> walkSuffix ".hs" dir
      skipAutogen = not . ("autogen" `isPrefixOf`) . GHC.moduleNameString
  liftIO $ filter skipAutogen . concat <$> traverse infer paths

inferModuleName :: GHC.GhcMonad m => FilePath -> m GHC.ModuleName
inferModuleName file = do
  dflags <- GHC.getSessionDynFlags
  f <- liftIO $ makeAbsolute file
  paths <- liftIO . traverse makeAbsolute $ GHC.importPaths dflags
  case find (`isPrefixOf` f) paths of
    Nothing -> error $ "could not find source root directory for " <> f
    Just dir -> pure $ inferModuleNameFor dir f

inferModuleNameFor :: FilePath -> FilePath -> GHC.ModuleName
inferModuleNameFor dir file =
  let name = dropWhile isPathSeparator . dropPrefix dir $ dropExtension file
      replace c = if isPathSeparator c then '.' else c
   in GHC.mkModuleName $ replace <$> name

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

-- from extra
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b

-- from extra
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)

-- from extra
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b
