{-# LANGUAGE ViewPatterns #-}

module HsInspect.Json where

import qualified Data.Text as T
import qualified GHC as GHC
import HsInspect.Sexp
import Json
import MonadUtils (mapSndM)
import Outputable (showSDoc)
import Util (mapFst)

encodeJson :: GHC.DynFlags -> JsonDoc -> String
encodeJson dflags j = showSDoc dflags . renderJSON $ j

sexpToJson :: Sexp -> Either String JsonDoc
sexpToJson SexpNil = Right JSNull
sexpToJson (toAList -> Just kvs) = JSObject . mapFst T.unpack <$> mapSndM sexpToJson kvs
sexpToJson (toList -> Just as) = JSArray <$> traverse sexpToJson as
sexpToJson (SexpCons _ _) = Left $ "cons cell has no JSON equivalent"
sexpToJson (SexpString s) = Right . JSString $ T.unpack s
sexpToJson (SexpSymbol s) = Right . JSString $ T.unpack s -- nobody said it had to roundtrip
sexpToJson (SexpInt i) = Right $ JSInt i
-- TODO write our own JSON repr to avoid a ghc dep and improve perf
