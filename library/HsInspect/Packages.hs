{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Packages (packages) where

import           BasicTypes (StringLiteral(..))
import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (isSuffixOf, nub, sort, (\\))
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           FastString
import           Finder (findImportedModule)
import qualified GHC
import           HscTypes (FindResult(..))
import           HsInspect.Sexp
import           HsInspect.Workarounds
import           Json
import           Module (Module(..), ModuleName, moduleNameString, unitIdString)
import           Packages (PackageState(..))
import           System.Directory (doesDirectoryExist, listDirectory)

-- Similar to packunused / weeder, but more reliable (and doesn't require a
-- separate -ddump-minimal-imports pass).
--
-- TODO support list of dirs not just one
packages :: GHC.GhcMonad m => FilePath -> m PkgSummary
packages dir = do
  -- We load all .hs files in dir, assuming they are the sources of the home
  -- module, but with a twist: we only parse the imports from external packages.
  -- To do this we have to unload the home modules as provided by parameters and
  -- filter then when parsing the imports section.
  args <- GHC.getTargets
  let homes = Set.fromList . catMaybes $ getModule <$> args

  srcs <- liftIO $ (filter (".hs" `isSuffixOf`)) <$> walk dir

  -- mods == homes. We could do two passes (ignore provided targets)
  (catMaybes -> mods, targets) <- unzip <$> traverse (importsOnly homes) srcs
  _ <- GHC.setTargets targets
  _ <- GHC.load $ GHC.LoadAllTargets

  imps <- nub . join <$> traverse getImports mods
  pkgs <- catMaybes <$> traverse (uncurry findPackage) imps
  let used = nub . sort $ pkgs
  dflags <- GHC.getSessionDynFlags
  let loaded = nub . sort . explicitPackages $ GHC.pkgState dflags
  pure $ PkgSummary used (loaded \\ used)

getModule :: GHC.Target -> Maybe ModuleName
getModule GHC.Target{GHC.targetId} = case targetId of
  GHC.TargetModule m -> Just m
  GHC.TargetFile _ _ -> Nothing

findPackage :: GHC.GhcMonad m => ModuleName -> Maybe FastString -> m (Maybe GHC.UnitId)
findPackage m mp = do
  env <- GHC.getSession
  res <- liftIO $ findImportedModule env m mp
  pure $ case res of
    Found _ (Module u _) -> Just $ u
    _ -> Nothing

getImports :: GHC.GhcMonad m => ModuleName -> m [(ModuleName, Maybe FastString)]
getImports m = do
  modSum <- GHC.getModSummary m
  pmod <- GHC.parseModule modSum
  tmod <- GHC.typecheckModule pmod
  case GHC.tm_renamed_source tmod of
    Nothing -> error $ "bad module: " ++ moduleNameString m
    Just (_, (GHC.unLoc <$>) -> imports, _, _) ->
      pure . catMaybes $ qModule <$> imports

qModule :: GHC.ImportDecl p -> Maybe (ModuleName, Maybe FastString)
qModule GHC.ImportDecl{GHC.ideclName, GHC.ideclPkgQual} = Just $
  (GHC.unLoc ideclName, qual)
  where qual = sl_fs <$> ideclPkgQual
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
qModule (GHC.XImportDecl _) = Nothing
#endif

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

