{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
module Medley.Types where

#if MIN_VERSION_GLASGOW_HASKELL(9,5,0,0)
import Data.Kind (Type)
#endif

import Prelude (Double, Int, String)

-- product
data Coord1 = Coordy1 Double Double

-- record
data Coord2 = Coordy2 { a :: Double, b :: Double}

-- sum
data Union = Wales String | England Int Int | Scotland

-- newtype
newtype Coord = C { foo :: Int }

-- polymorphic product
data Things a b = T a b Double

-- higher kinded polymorphic record
#if MIN_VERSION_GLASGOW_HASKELL(9,5,0,0)
data Logger (m :: Type -> Type) = Logger { log :: String -> m () }
#else
data Logger (m :: * -> *) = Logger { log :: String -> m () }
#endif
-- polymorphic sum
data Action a = Admin [a] String | User a String

-- infix product
data InfixProd = String :| String

-- infix sum
data InfixSum = String :|| String
              | String :||| String

