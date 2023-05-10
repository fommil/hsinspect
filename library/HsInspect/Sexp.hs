{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Very minimal ADT for outputting some S-Expressions.
module HsInspect.Sexp where

import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import GHC.Data.FastString (unpackFS)
import GHC.Utils.Json (escapeJsonString)
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Unit.Info (PackageName(..), PackageId(..))
#else
import FastString (unpackFS)
import Json (escapeJsonString)
import Module (ModuleName, moduleNameString)
import PackageConfig (PackageName(..), SourcePackageId(..))
#endif

data Sexp
  = SexpCons Sexp Sexp
  | SexpNil
  | SexpString Text
  | SexpSymbol Text
  | SexpInt Int

list :: [Sexp] -> Sexp
list = foldr SexpCons SexpNil

toList :: Sexp -> Maybe [Sexp]
toList SexpNil = Just []
toList (SexpCons a b) = (a :) <$> toList b
toList _ = Nothing

instance IsString Sexp where
  fromString = SexpSymbol . T.pack

alist :: [(Sexp, Sexp)] -> Sexp
alist els = list $ mkEl =<< els
  where
    mkEl (k, v) = [SexpCons k v]

toAList :: Sexp -> Maybe [(Text, Sexp)]
toAList SexpNil = Just []
toAList (SexpCons (SexpCons (SexpSymbol k) v) rest) = ((k, v) :) <$> toAList rest
toAList _ = Nothing

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp Sexp where
  toSexp = id

instance ToSexp Text where
  toSexp = SexpString

instance ToSexp Bool where
  toSexp False = SexpNil
  toSexp True = SexpSymbol "t"

instance ToSexp Int where
  toSexp = SexpInt

instance ToSexp a => ToSexp [a] where
  toSexp as = list $ toSexp <$> as

instance (ToSexp a1, ToSexp a2) => ToSexp (a1, a2) where
  toSexp (a1, a2) = list [toSexp a1, toSexp a2]

instance (ToSexp a1, ToSexp a2, ToSexp a3) => ToSexp (a1, a2, a3) where
  toSexp (a1, a2, a3) = list [toSexp a1, toSexp a2, toSexp a3]

instance ToSexp a => ToSexp (Maybe a) where
  toSexp (Just a) = toSexp a
  toSexp Nothing = SexpNil

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
instance ToSexp PackageId where
  toSexp (PackageId fs) = SexpString . T.pack $ unpackFS fs
#else
instance ToSexp SourcePackageId where
  toSexp (SourcePackageId fs) = SexpString . T.pack $ unpackFS fs
#endif

instance ToSexp ModuleName where
  toSexp = SexpString . T.pack . moduleNameString

instance ToSexp PackageName where
  toSexp (PackageName fs) = SexpString . T.pack $ unpackFS fs

filterNil :: Sexp -> Sexp
filterNil SexpNil = SexpNil
filterNil (SexpCons (SexpCons (SexpSymbol _) SexpNil) rest) = filterNil rest
filterNil (SexpCons car cdr) = (SexpCons (filterNil car) (filterNil cdr))
filterNil (SexpString s) = SexpString s
filterNil (SexpSymbol s) = SexpSymbol s
filterNil (SexpInt i) = SexpInt i

render :: Sexp -> Text
render SexpNil = "nil"
render (toList -> Just ss) = "(" <> (T.intercalate " " $ render <$> ss) <> ")\n"
render (SexpCons a b) = "(" <> render a <> " . " <> render b <> ")\n"
render (SexpString s) = "\"" <> (T.pack . escapeJsonString $ T.unpack s) <> "\""
render (SexpSymbol a) = T.pack . escapeJsonString $ T.unpack a
render (SexpInt i) = T.pack $ show i
-- TODO write our own escapeString to avoid a ghc dep and improve perf
