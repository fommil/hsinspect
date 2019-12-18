{-# LANGUAGE ViewPatterns #-}

module HsInspect.Json where

import qualified GHC as GHC
import HsInspect.Sexp
import Json
import MonadUtils (mapSndM)
import Outputable (showSDoc)

encodeJson :: GHC.DynFlags -> JsonDoc -> String
encodeJson dflags j = showSDoc dflags . renderJSON $ j

sexpToJson :: Sexp -> Either String JsonDoc
sexpToJson SexpNil = Right JSNull
sexpToJson (toAList -> Just kvs) = JSObject <$> mapSndM sexpToJson kvs
sexpToJson (toList -> Just as) = JSArray <$> traverse sexpToJson as
sexpToJson (SexpCons _ _) = Left $ "cons cell has no JSON equivalent"
sexpToJson (SexpString s) = Right $ JSString s
sexpToJson (SexpSymbol s) = Right $ JSString s -- nobody said it had to roundtrip
