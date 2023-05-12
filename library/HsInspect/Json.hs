{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Json where

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Utils.Json as GHC
import qualified GHC.Utils.Monad as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Utils.Misc as GHC
#else
import qualified Json as GHC
import qualified MonadUtils as GHC
import qualified Outputable as GHC
import qualified Util as GHC
#endif

import qualified Data.Text as T
import HsInspect.Sexp

encodeJson :: GHC.JsonDoc -> String
encodeJson j = GHC.showSDocUnsafe . GHC.renderJSON $ j

sexpToJson :: Sexp -> Either String GHC.JsonDoc
sexpToJson SexpNil = Right GHC.JSNull
sexpToJson (toAList -> Just kvs) = GHC.JSObject . GHC.mapFst T.unpack <$> GHC.mapSndM sexpToJson kvs
sexpToJson (toList -> Just as) = GHC.JSArray <$> traverse sexpToJson as
sexpToJson (SexpCons _ _) = Left $ "cons cell has no JSON equivalent"
sexpToJson (SexpString s) = Right . GHC.JSString $ T.unpack s
sexpToJson (SexpSymbol s) = Right . GHC.JSString $ T.unpack s -- nobody said it had to roundtrip
sexpToJson (SexpInt i) = Right $ GHC.JSInt i
-- TODO write our own JSON repr to avoid a ghc dep and improve perf
