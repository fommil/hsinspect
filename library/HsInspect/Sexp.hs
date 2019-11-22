{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Sexp where

import Data.List (intercalate)
import Data.String
import Json (escapeJsonString)

data Sexp
  = SexpCons Sexp Sexp
  | SexpNil
  | SexpString String
  | SexpSymbol String

list :: [Sexp] -> Sexp
list = foldr SexpCons SexpNil

toAList :: Sexp -> Maybe [(String, Sexp)]
toAList SexpNil = Just []
toAList (SexpCons (SexpCons (SexpSymbol k) v) rest) = ((k, v) :) <$> toAList rest
toAList _ = Nothing

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

attrs :: [(String, Sexp)] -> Sexp
attrs els = list $ mkEl =<< els
  where
    mkEl (k, v) = [SexpSymbol k, v]

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp Sexp where
  toSexp = id

instance ToSexp String where
  toSexp s = SexpString s

instance ToSexp a => ToSexp [a] where
  toSexp as = list $ toSexp <$> as

instance ToSexp a => ToSexp (Maybe a) where
  toSexp (Just a) = toSexp a
  toSexp Nothing = SexpNil

render :: Sexp -> String
render SexpNil = "nil"
render (toList -> Just ss) = "(" ++ (intercalate " " $ render <$> ss) ++ ")\n"
render (SexpCons a b) = "(" ++ render a ++ " . " ++ render b ++ ")\n"
render (SexpString s) = "\"" ++ escapeJsonString s ++ "\""
render (SexpSymbol a) = escapeJsonString a

encode :: ToSexp a => a -> String
encode = render . toSexp
