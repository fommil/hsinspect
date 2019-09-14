{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Packages (packages) where

import           BasicTypes (StringLiteral(..))
import           Control.Monad.IO.Class (liftIO)
import           Data.List (isSuffixOf, nub, sort, (\\))
import           Data.Maybe (catMaybes)
import           FastString
import           Finder (findImportedModule)
import qualified GHC
import           HscTypes (FindResult(..))
import           HsInspect.Sexp
import           HsInspect.Workarounds
import           Json
import           Module (Module(..), unitIdString)
import           Packages (PackageState(..))
import           System.Directory (doesDirectoryExist, listDirectory)

packages :: GHC.GhcMonad m => FilePath -> m PkgSummary
packages dir = do
  orig <- GHC.getTargets
  srcs <- liftIO $ (filter (".hs" `isSuffixOf`)) <$> walk dir
  let parseAndReset s = getImports s <* GHC.setTargets orig
  imps <- nub <$> concatMapM parseAndReset srcs
  pkgs <- catMaybes <$> traverse (uncurry findPackage) imps
  let used = nub . sort $ pkgs
  dflags <- GHC.getSessionDynFlags
  let loaded = nub . sort . explicitPackages $ GHC.pkgState dflags
  pure $ PkgSummary used (loaded \\ used)

findPackage :: GHC.GhcMonad m => GHC.ModuleName -> Maybe FastString -> m (Maybe GHC.UnitId)
findPackage m mp = do
  env <- GHC.getSession
  res <- liftIO $ findImportedModule env m mp
  pure $ case res of
    Found _ (Module u _) -> Just $ u
    _ -> Nothing

getImports :: GHC.GhcMonad m => FilePath -> m [(GHC.ModuleName, Maybe FastString)]
getImports file = do
  (m, target) <- importsOnly file
  GHC.removeTarget $ GHC.TargetModule m
  GHC.addTarget target

  _ <- GHC.load $ GHC.LoadUpTo m
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod

  case GHC.tm_renamed_source tmod of
    Nothing -> error $ "bad file: " ++ file
    Just (_, (GHC.unLoc <$>) -> imports, _, _) ->
      pure . catMaybes $ qModule <$> imports

qModule :: GHC.ImportDecl p -> Maybe (GHC.ModuleName, Maybe FastString)
qModule GHC.ImportDecl{GHC.ideclName, GHC.ideclPkgQual} = Just $
  (GHC.unLoc ideclName, qual)
  where qual = sl_fs <$> ideclPkgQual
qModule _ = Nothing -- TODO CPP for 8.6.5+

walk :: FilePath -> IO [FilePath]
walk dir = do
  isDir <- doesDirectoryExist dir
  if isDir
  then do fs <- listDirectory dir
          let base = dir <> "/"
              qfs = (base <>) <$> fs
          concatMapM walk qfs
  else pure [dir]

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

data PkgSummary = PkgSummary [GHC.UnitId] [GHC.UnitId]
  deriving (Eq, Ord)

instance ToSexp PkgSummary where
  toSexp (PkgSummary used unused) =
    alist [ ("used", toS used)
          , ("unused", toS unused) ]
    where toS ids = toSexp $ unitIdString <$> ids

instance ToJson PkgSummary where
  json (PkgSummary used unused) =
    JSObject [ ("used", toJ used)
             , ("unused", toJ unused) ]
    where toJ ids = JSArray $ JSString . unitIdString <$> ids

