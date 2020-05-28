{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Abstraction of the hsinspect binary.
--
-- We intentionally do not depend on the hsinspect library because by decoupling
-- the user can install one shared hsinspect-lsp binary for all projects, with
-- only ghcflags / hsinspect setup per project.
module HsInspect.LSP.HsInspect where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics
import HsInspect.LSP.Context
import HsInspect.LSP.Util
import qualified System.Log.Logger as L

hsinspect_imports :: Context -> FilePath -> ExceptT String IO [Import]
hsinspect_imports ctx hs = hsinspect_raw ctx ["imports", hs]

hsinspect_index :: Context -> ExceptT String IO [Package]
hsinspect_index ctx = hsinspect_raw ctx ["index"]

hsinspect_raw :: FromJSON a => Context -> [String] -> ExceptT String IO a
hsinspect_raw Context{hsinspect, package_dir, ghcflags, ghcpath} args = do
  liftIO $ L.debugM "haskell-lsp" $ "hsinspect-lsp:cwd:" <> package_dir
  stdout <- shell hsinspect (args <> ["--json", "--"] <> ghcflags) (Just package_dir) (Just ghcpath) [("GHC_ENVIRONMENT", "-")]
  ExceptT . pure . eitherDecodeStrict' $ C.pack stdout

data Import = Import
  { _local :: Maybe Text
  , _qual :: Maybe Text
  , _full :: Text
  } deriving (Eq, Show, Generic)

data Package = Package
  { _srcid :: Maybe Text
  , _inplace :: Maybe Text -- bad Bool encoding
  , _modules :: Maybe [Module]
  , _haddocks :: Maybe [FilePath]
  } deriving (Eq, Show, Generic)

data Module = Module
  { _module :: Text
  , _ids :: Maybe [Entry]
  } deriving (Eq, Show, Generic)

data Entry =
    Id { _export :: Maybe Exported
       , _name :: Text
       , _type :: Text }
  | Con { _export :: Maybe Exported
        , _name :: Text
        , _type :: Text }
  | Pat { _export :: Maybe Exported
        , _name :: Text
        , _type :: Text }
  | TyCon { _export :: Maybe Exported
          , _type :: Text
          , _flavour :: Text }
  deriving (Eq, Show, Generic)

data Exported = Exported
  { _srcid :: (Maybe Text)
  , _module :: Text
  } deriving (Eq, Show, Generic)

jsonConventions :: Options
jsonConventions = defaultOptions
  { fieldLabelModifier = dropWhile ('_' ==)
  , constructorTagModifier = map toLower
  , sumEncoding = TaggedObject "class" ""
  , omitNothingFields = True
  }

-- TODO DerivingVia this boilerplate away
instance FromJSON Import where
  parseJSON = genericParseJSON jsonConventions

instance FromJSON Package where
  parseJSON = genericParseJSON jsonConventions

instance FromJSON Module where
  parseJSON = genericParseJSON jsonConventions

instance FromJSON Entry where
  parseJSON = genericParseJSON jsonConventions

instance FromJSON Exported where
  parseJSON = genericParseJSON jsonConventions
