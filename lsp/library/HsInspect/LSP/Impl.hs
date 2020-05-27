
-- Features of https://microsoft.github.io/language-server-protocol/specification.html
module HsInspect.LSP.Impl where

import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Except (throwE)
import Data.Cache (Cache)
import qualified Data.Cache as C
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified FastString as GHC
import qualified GHC as GHC
import GHC.Paths (libdir)
import HsInspect.LSP.Context
import HsInspect.LSP.HsInspect
import qualified Lexer as GHC
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC

-- TODO consider more advanced cache keys, e.g. Import could be cached on a
--      checksum of the file's header plus a checksum of the .ghc.* files.
data Caches = Caches
  (Cache FilePath Context) -- ^ by the package_dir
  (Cache FilePath [Import]) -- ^ by source filename

cachedContext :: Caches -> BuildTool -> FilePath -> ExceptT String IO Context
cachedContext (Caches cache _) tool file = do
  let discover = mkDiscoverContext tool
  key <- discoverPackageDir discover file
  let work = do
        ctx <- findContext discover file
        liftIO $ C.insert cache key ctx
        pure ctx
  fromMaybeM work . liftIO $ C.lookup cache key

cachedImports :: Caches -> Context -> FilePath -> ExceptT String IO [Import]
cachedImports (Caches _ cache) ctx file =
  C.fetchWithCache cache file $ imports mkHsInspect ctx

-- FIXME return all hits, not just the first one
-- TODO docs / parameter info
signatureHelpProvider :: Caches -> BuildTool -> FilePath -> (Int, Int) -> ExceptT String IO [Text]
signatureHelpProvider caches tool file position = do
  ctx <- cachedContext caches tool file
  symbols <- cachedImports caches ctx file
  sym <- symbolAtPoint file position

  -- TODO include type information by consulting the index

  let
    matcher imp =
      if _local imp == Just sym || _qual imp == Just sym || _full imp == sym
      then Just $ _full imp
      else Nothing

  pure $ mapMaybe matcher symbols

-- c.f. haskell-tng--hsinspect-symbol-at-point
--
-- TODO consider replacing this (inefficient) ghc api usage with a regexp or
-- calling the specific lexer functions directly for the symbols we support.
symbolAtPoint :: FilePath -> (Int, Int) -> ExceptT String IO Text
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
      let containsPoint :: GHC.SrcSpan -> Bool
          containsPoint (GHC.UnhelpfulSpan _) = False
          containsPoint (GHC.RealSrcSpan s) = GHC.containsSpan s point
       in maybe (throwE "could not find a token") (pure . T.pack . snd) .
            L.find (containsPoint . GHC.getLoc . fst) $
            GHC.addSourceToTokens startLoc buf ts

    _ -> throwE "lexer error" -- TODO getErrorMessages
