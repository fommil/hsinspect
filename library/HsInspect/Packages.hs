{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Packages (packages) where

import           BasicTypes (StringLiteral(..))
import           Control.Monad.IO.Class (liftIO)
import           Data.List (isSuffixOf, nub, sort)
import           Data.Maybe (catMaybes)
import           FastString
import           Finder (findImportedModule)
import qualified GHC
import           HscTypes (FindResult(..))
import           HsInspect.Sexp
import           HsInspect.Workarounds
import           Json
import           Module (Module(..), unitIdFS)
import           System.Directory (doesDirectoryExist, listDirectory)

packages :: GHC.GhcMonad m => FilePath -> m [Hit]
packages dir = do
  srcs <- liftIO $ (filter (".hs" `isSuffixOf`)) <$> walk dir
  imps <- nub <$> concatMapM getImports srcs
  let getPkg (Left m) = findPackage m
      getPkg (Right p) = pure $ Just p
  pkgs <- catMaybes <$> traverse getPkg imps
  pure $ Hit . unpackFS <$> (nub . sort $ pkgs)

findPackage :: GHC.GhcMonad m => GHC.ModuleName -> m (Maybe FastString)
findPackage m = do
  env <- GHC.getSession
  res <- liftIO $ findImportedModule env m Nothing
  pure $ case res of
    Found _ (Module (unitIdFS -> p) _) -> Just p
    _ -> Nothing

getImports :: GHC.GhcMonad m => FilePath -> m [Either GHC.ModuleName FastString]
getImports file = do
  orig <- GHC.getTargets

  (m, target) <- importsOnly file
  GHC.removeTarget $ GHC.TargetModule m
  GHC.addTarget target

  _ <- GHC.load $ GHC.LoadUpTo m
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod

  GHC.setTargets orig
  -- TODO do we need to unload?

  case GHC.tm_renamed_source tmod of
    Nothing -> error $ "bad file: " ++ file
    Just (_, (GHC.unLoc <$>) -> imports, _, _) ->
      pure . catMaybes $ moduleOrPackage <$> imports

moduleOrPackage :: GHC.ImportDecl p -> Maybe (Either GHC.ModuleName FastString)
moduleOrPackage GHC.ImportDecl{GHC.ideclName, GHC.ideclPkgQual} = pure $
  case ideclPkgQual of
    Just pkg -> Right $ sl_fs pkg
    Nothing  -> Left $ GHC.unLoc ideclName
moduleOrPackage _ = Nothing

walk :: FilePath -> IO [FilePath]
walk dir = do
  fs <- listDirectory dir
  let qfs = ((dir <> "/") <>) <$> fs
  (dirs, files) <- partitionM doesDirectoryExist qfs
  (files <>) <$> (concatMapM walk dirs)

-- from extra
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x : xs) = do
  res <- f x
  (as, bs) <- partitionM f xs
  pure ([x | res] ++ as, [x | not res] ++ bs)

-- from extra
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
    where f x xs = do
            x' <- op x
            if null x'
            then xs
            else do
              xs' <- xs
              pure $ x' ++ xs'

data Hit = Hit String
  deriving (Eq, Ord)

instance ToSexp Hit where
  toSexp (Hit txt) = toSexp txt

instance ToJson Hit where
  json (Hit txt) = JSString txt
