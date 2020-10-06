{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- Features of https://microsoft.github.io/language-server-protocol/specification.html
module HsInspect.LSP.Impl where

import Control.Monad (guard)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Except (throwE)
import Data.Cache (Cache)
import qualified Data.Cache as C
import Data.List (nub)
import Data.List.Extra (firstJust)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import qualified FastString as GHC
import qualified GHC as GHC
import GHC.Paths (libdir)
import HsInspect.Context
import HsInspect.LSP.HsInspect
import qualified Lexer as GHC
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC
import System.Directory (findExecutablesInDirectories)
import System.FilePath (splitSearchPath)
import System.FilePath (takeDirectory)

-- TODO consider invalidation strategies, e.g. Import could use the file's
--      header, and the Context use the .ghc.* files. The index might also want
--      to hash the .ghc.flags contents plus any changes to exported symbols in
--      the current package. But beware that often it is better to have a stale
--      cache and respond with *something* than to be slow and redo the work.
data Caches = Caches
  (Cache FilePath (HsInspectBin, Context)) -- ^ by source root
  (Cache FilePath [Import]) -- ^ by source filename
  (Cache FilePath [Package]) -- ^ by source root

-- TODO the index could use a data structure that is faster to search

discoverHsInspect :: Text -> ExceptT String IO HsInspectBin
discoverHsInspect path = do
  let dirs = splitSearchPath $ T.unpack path
  found <- liftIO $ findExecutablesInDirectories dirs "hsinspect"
  case found of
    [] -> throwE help_hsinspect
    exe : _ -> pure $ HsInspectBin exe

help_hsinspect :: String
help_hsinspect = "The hsinspect binary has not been installed for this package. \
                 \See https://gitlab.com/tseenshe/hsinspect#installation for more details."

cachedContext :: Caches -> FilePath -> ExceptT String IO (HsInspectBin, Context)
cachedContext (Caches cache _ _) file = do
  key <- takeDirectory <$> discoverGhcflags file
  let work = do
        ctx <- findContext file
        bin <- discoverHsInspect $ ghcpath ctx
        liftIO $ C.insert cache key (bin, ctx)
        pure (bin, ctx)
  fromMaybeM work . liftIO $ C.lookup cache key

cachedImports :: Caches -> HsInspectBin -> Context -> FilePath -> ExceptT String IO [Import]
cachedImports (Caches _ cache _) bin ctx file =
  C.fetchWithCache cache file $ hsinspect_imports bin ctx

cachedIndex :: Caches -> HsInspectBin -> Context -> ExceptT String IO [Package]
cachedIndex (Caches _ _ cache) bin ctx =
  C.fetchWithCache cache (srcdir ctx) $ \_ -> hsinspect_index bin ctx

-- only lookup the index, don't try to populate it
cachedIndex' :: Caches -> Context -> ExceptT String IO [Package]
cachedIndex' (Caches _ _ cache) ctx = liftIO $
  fromMaybe [] <$> C.lookup cache (srcdir ctx)

-- zero indexed
data Span = Span Int Int Int Int -- line, col, line, col
  deriving Show

getFullModuleName :: Text -> [Import] -> Maybe Text
getFullModuleName query imports =
  case results of
    [] -> Nothing
    result:_ -> Just result
  where results = mapMaybe extractModule imports
        extractModule imp = do
          qualName <- _qual imp
          guard $ T.isPrefixOf query qualName
          let fullImport = _full imp
          let (T.init -> module', _) =
                T.breakOnEnd "." fullImport
          return $ module'

findType :: Text -> [Package] -> Maybe Text
findType qual pkgs = listToMaybe $ do
  let (T.init -> module', sym) = T.breakOnEnd "." qual
  let flatten Nothing = []
      flatten (Just as) = as
  pkg <- pkgs
  Module module'' entries <- flatten . _modules $ pkg
  if module'' /= module'
  then []
  else do
    e <- flatten entries
    let matcher name typ = if name == sym && name /= typ then [typ] else []
    case e of
      Id _ name typ -> matcher name typ
      Con _ name typ -> matcher name typ
      Pat _ name typ -> matcher name typ
      TyCon _ _ _ -> []

findNameAndTypes :: Text -> [Package] -> [(Text, Text, Text)]
findNameAndTypes qual pkgs = do
  let (_, sym) = T.breakOnEnd "." qual
  let flatten Nothing = []
      flatten (Just as) = as
  pkg <- pkgs
  Module m entries <- flatten . _modules $ pkg
  e <- flatten entries
  let matcher name typ = if T.isPrefixOf sym name then [(name, typ, m)] else []
  case traceShowId e of
    Id _ name typ -> matcher name typ
    Con _ name typ -> matcher name typ
    Pat _ name typ -> matcher name typ
    TyCon _ _ _ -> []


hoverProvider :: Caches -> FilePath -> (Int, Int) -> ExceptT String IO (Maybe (Span, Text))
hoverProvider caches file position = do
  (bin, ctx) <- cachedContext caches file
  symbols <- cachedImports caches bin ctx file
  index <- cachedIndex' caches ctx
  found <- symbolAtPoint file position
  pure $ case traceShow found found of
    Nothing -> Nothing
    Just (range, sym) ->
      let matcher imp = if _local imp == Just sym || _qual imp == Just sym || _full imp == sym
                        then Just $ case findType (_full imp) index of
                          Just typ -> _full imp <> " :: " <> typ
                          Nothing -> _full imp
                        else Nothing
       in (range,) <$> firstJust matcher symbols

-- TODO use the index to add optional type information
completionProvider :: Caches -> FilePath -> Text -> (Int, Int) -> ExceptT String IO [(Text, Maybe Text)]
completionProvider caches file contents position = do
  (bin, ctx) <- cachedContext caches file
  symbols <- cachedImports caches bin ctx file
  index <- cachedIndex' caches ctx
  found <- symbolAtVirtualPoint file contents position
  pure $ case traceShow (found, symbols) found of
    Nothing -> []
    Just (_, sym) -> do
      let findNameAndType :: Text -> Import -> (Text, Maybe Text)
          findNameAndType name imp =
            case findType (_full imp) index of
              Just typ -> (name, Just typ)
              Nothing -> (name, Nothing)

          matcher :: (Import -> Maybe Text) -> Import -> Maybe (Text, Maybe Text)
          matcher key imp = do
            pref <- key imp
            if T.isPrefixOf sym pref
              then return $ findNameAndType pref imp
              else Nothing

      let localMatches = map (matcher _local) symbols
      let qualMatches = map (matcher _qual) symbols
      let fullMatches = map (matcher (Just . _full)) symbols

      nub $ catMaybes $ localMatches <> qualMatches <> fullMatches

-- c.f. haskell-tng--hsinspect-symbol-at-point
--
-- TODO consider replacing this (inefficient) ghc api usage with a regexp or
-- calling the specific lexer functions directly for the symbols we support.
symbolAtPoint :: FilePath -> (Int, Int) -> ExceptT String IO (Maybe (Span, Text))
symbolAtPoint file (line, col) = do
  buf' <- liftIO $ GHC.hGetStringBuffer file
  -- TODO for performance, and language extension reliability, find out how to
  --      start the lexer on the correct line, to avoid lexing stuff we don't
  --      care about.
  buf <- maybe (throwE "line doesn't exist") pure $ GHC.atLine 1 buf'
  let file' = GHC.mkFastString file
      startLoc = GHC.mkRealSrcLoc file' 1 1
      -- add 1 to the column because GHC.containsSpan doesn't seem to like it
      -- when the point is on the very first character of the span and if we
      -- must pick between the first or last char, we prefer the first.
      point = GHC.realSrcLocSpan $ GHC.mkRealSrcLoc file' line (col + 1)
  -- TODO construct the real dflags from the Context (remove libdir dependency)
  dflags <- liftIO . GHC.runGhc (Just libdir) $ GHC.getSessionDynFlags
  -- lexTokenStream :: StringBuffer -> RealSrcLoc -> DynFlags -> ParseResult [Located Token]
  case GHC.lexTokenStream buf startLoc dflags of
    GHC.POk _ ts  ->
      let containsPoint :: (GHC.Located GHC.Token, String) -> Maybe (Span, Text)
          containsPoint ((GHC.L (GHC.UnhelpfulSpan _) _), _) = Nothing
          containsPoint ((GHC.L (GHC.RealSrcSpan s) _), txt) =
            if GHC.containsSpan s point then Just (toSpan s, T.pack txt) else Nothing
          toSpan src = Span
            (GHC.srcSpanStartLine src - 1)
            (GHC.srcSpanStartCol src - 1)
            (GHC.srcSpanEndLine src - 1)
            (GHC.srcSpanEndCol src - 1)

       in pure . firstJust containsPoint $ GHC.addSourceToTokens startLoc buf ts

    _ -> throwE "lexer error" -- TODO getErrorMessages

symbolAtVirtualPoint :: FilePath -> Text -> (Int, Int) -> ExceptT String IO (Maybe (Span, Text))
symbolAtVirtualPoint file contents (line, col) = do
  let buf' = GHC.stringToStringBuffer $ T.unpack contents
  buf <- maybe (throwE "line doesn't exist") pure $ GHC.atLine 1 buf'
  let file' = GHC.mkFastString file
      startLoc = GHC.mkRealSrcLoc file' 1 1
      -- when you autocomplete you're at the end of word
      -- you do not want suggestions for the next column
      point = GHC.realSrcLocSpan $ GHC.mkRealSrcLoc file' line col
  -- TODO construct the real dflags from the Context (remove libdir dependency)
  dflags <- liftIO . GHC.runGhc (Just libdir) $ GHC.getSessionDynFlags
  -- lexTokenStream :: StringBuffer -> RealSrcLoc -> DynFlags -> ParseResult [Located Token]
  case GHC.lexTokenStream buf startLoc dflags of
    GHC.POk _ ts  ->
      let containsPoint :: (GHC.Located GHC.Token, String) -> Maybe (Span, Text)
          containsPoint ((GHC.L (GHC.UnhelpfulSpan _) _), _) = Nothing
          containsPoint ((GHC.L (GHC.RealSrcSpan s) _), txt) =
            if GHC.containsSpan s point then Just (toSpan s, T.pack txt) else Nothing
          toSpan src = Span
            (GHC.srcSpanStartLine src - 1)
            (GHC.srcSpanStartCol src - 1)
            (GHC.srcSpanEndLine src - 1)
            (GHC.srcSpanEndCol src - 1)

       in pure . firstJust containsPoint $ GHC.addSourceToTokens startLoc buf ts

    _ -> throwE "lexer error" -- TODO getErrorMessages
