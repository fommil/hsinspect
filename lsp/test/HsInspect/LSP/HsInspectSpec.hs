{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.LSP.HsInspectSpec where

import Control.Exception (throwIO)
import Data.Aeson
import HsInspect.LSP.HsInspect
import Test.Hspec

spec :: Spec
spec = do
  let dir = "../tests/medley/library/"
      parse :: HasCallStack => FromJSON a => FilePath -> IO a
      parse p = do
        res <- eitherDecodeFileStrict' (dir <> p)
        case res of
          Left reason -> throwIO $ userError reason
          Right a -> pure a

  it "should parse 'imports' results" $ do
    decoded :: [Import] <- parse "Medley/Wibble.hs.ghc-8.8.3.imports.json"
    decoded `shouldSatisfy` (not . null)

  it "should parse 'index' results" $ do
    decoded :: [Package] <- parse "ghc-8.8.3.index.json"
    decoded `shouldSatisfy` (not . null)
