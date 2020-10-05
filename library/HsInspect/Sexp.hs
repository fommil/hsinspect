{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Very minimal ADT for outputting some S-Expressions.
module HsInspect.Sexp where

import Data.List (intercalate)
import Data.String
import FastString (unpackFS)
import Json (escapeJsonString)
import Module (ModuleName, moduleNameString)
import PackageConfig (PackageName(..), SourcePackageId(..))

data Sexp
  = SexpCons Sexp Sexp
  | SexpNil
  | SexpString String
  | SexpSymbol String

list :: [Sexp] -> Sexp
list = foldr SexpCons SexpNil

toList :: Sexp -> Maybe [Sexp]
toList SexpNil = Just []
toList (SexpCons a b) = (a :) <$> toList b
toList _ = Nothing

instance IsString Sexp where
  fromString = SexpSymbol

alist :: [(Sexp, Sexp)] -> Sexp
alist els = list $ mkEl =<< els
  where
    mkEl (k, v) = [SexpCons k v]

toAList :: Sexp -> Maybe [(String, Sexp)]
toAList SexpNil = Just []
toAList (SexpCons (SexpCons (SexpSymbol k) v) rest) = ((k, v) :) <$> toAList rest
toAList _ = Nothing

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp Sexp where
  toSexp = id

-- TODO this is a horrible instance because it requires lang extensions and is
-- otherwise slow. Prefer Text everywhere (which needs a convenience FastString
-- -> Text)
instance ToSexp String where
  toSexp s = SexpString s

instance ToSexp Bool where
  toSexp False = SexpNil
  toSexp True = SexpSymbol "t"

instance ToSexp a => ToSexp [a] where
  toSexp as = list $ toSexp <$> as

instance ToSexp a => ToSexp (Maybe a) where
  toSexp (Just a) = toSexp a
  toSexp Nothing = SexpNil

instance ToSexp SourcePackageId where
  toSexp (SourcePackageId fs) = SexpString $ unpackFS fs

instance ToSexp ModuleName where
  toSexp = SexpString . moduleNameString

instance ToSexp PackageName where
  toSexp (PackageName fs) = SexpString $ unpackFS fs

filterNil :: Sexp -> Sexp
filterNil SexpNil = SexpNil
filterNil (SexpCons (SexpCons (SexpSymbol _) SexpNil) rest) = filterNil rest
filterNil (SexpCons car cdr) = (SexpCons (filterNil car) (filterNil cdr))
filterNil (SexpString s) = SexpString s
filterNil (SexpSymbol s) = SexpSymbol s

render :: Sexp -> String
render SexpNil = "nil"
render (toList -> Just ss) = "(" ++ (intercalate " " $ render <$> ss) ++ ")\n"
render (SexpCons a b) = "(" ++ render a ++ " . " ++ render b ++ ")\n"
render (SexpString s) = "\"" ++ escapeJsonString s ++ "\""
render (SexpSymbol a) = escapeJsonString a
