{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Json where

import qualified Data.Text as T
import HsInspect.Sexp
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import GHC.Utils.Json
import GHC.Utils.Monad (mapSndM)
import GHC.Utils.Outputable (showSDocUnsafe)
import GHC.Utils.Misc (mapFst)
#else
import Json
import MonadUtils (mapSndM)
import Outputable (showSDocUnsafe)
import Util (mapFst)
#endif

encodeJson :: JsonDoc -> String
encodeJson j = showSDocUnsafe . renderJSON $ j

sexpToJson :: Sexp -> Either String JsonDoc
sexpToJson SexpNil = Right JSNull
sexpToJson (toAList -> Just kvs) = JSObject . mapFst T.unpack <$> mapSndM sexpToJson kvs
sexpToJson (toList -> Just as) = JSArray <$> traverse sexpToJson as
sexpToJson (SexpCons _ _) = Left $ "cons cell has no JSON equivalent"
sexpToJson (SexpString s) = Right . JSString $ T.unpack s
sexpToJson (SexpSymbol s) = Right . JSString $ T.unpack s -- nobody said it had to roundtrip
sexpToJson (SexpInt i) = Right $ JSInt i
-- TODO write our own JSON repr to avoid a ghc dep and improve perf
