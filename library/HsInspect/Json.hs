{-# OPTIONS_GHC -Wno-orphans #-}

module HsInspect.Json where

import qualified GHC as GHC
import Json
import Outputable (defaultUserStyle, initSDocContext, runSDoc)

encodeJson :: ToJson a => GHC.DynFlags -> a -> String
encodeJson dflags as = show . flip runSDoc ctx . renderJSON . json $ as
  where ctx = initSDocContext dflags $ defaultUserStyle dflags

instance ToJson a => ToJson (Maybe a) where
  json Nothing = JSNull
  json (Just a) = json a

instance ToJson a => ToJson [a] where
  json as = JSArray $ json <$> as


