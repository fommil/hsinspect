{-# LANGUAGE KindSignatures #-}
module Medley.Types where

import Prelude (Double, Int, String)

-- product
data Coord1 = Coordy1 Double Double

-- record
data Coord2 = Coordy2 { a :: Double, b :: Double}

-- sum
data Union = Wales String | England Int Int | Scotland

-- polymorphic product
data Things a b = T a b Double

-- higher kinded polymorphic record
data Logger (m :: * -> *) = Logger { log :: String -> m () }

-- polymorphic sum
data Action a = Admin [a] String | User a String

-- infix product
data InfixProd = String :| String

-- infix sum
data InfixSum = String :|| String
              | String :||| String

