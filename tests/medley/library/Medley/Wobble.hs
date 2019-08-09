{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Medley.Wobble where

-- can't use "base" + Prelude due to https://gitlab.haskell.org/ghc/ghc/issues/17045
import           "contravariant" Data.Functor.Contravariant.Divisible (Divisible,
                                                                       conquered)

wobble :: Divisible m => m ()
wobble = conquered

