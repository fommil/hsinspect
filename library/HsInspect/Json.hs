{-# LANGUAGE ViewPatterns #-}

module HsInspect.Json where

import qualified GHC as GHC
import HsInspect.Sexp
import Json
import MonadUtils (mapSndM)
import Outputable (defaultUserStyle, initSDocContext, runSDoc)

encodeJson :: GHC.DynFlags -> JsonDoc -> String
encodeJson dflags j = show . flip runSDoc ctx . renderJSON $ j
  where ctx = initSDocContext dflags $ defaultUserStyle dflags

sexpToJson :: Sexp -> Either String JsonDoc
sexpToJson sexp = case sexp of
  SexpNil -> Right JSNull
  (toAList -> Just kvs) -> JSObject <$> mapSndM sexpToJson kvs
  (toList -> Just as) -> JSArray <$> traverse sexpToJson as
  (SexpCons _ _) -> Left $ "cons cell has no JSON equivalent"
  (SexpString s) -> Right $ JSString s
  (SexpSymbol s) -> Right $ JSString s -- nobody said it had to roundtrip
