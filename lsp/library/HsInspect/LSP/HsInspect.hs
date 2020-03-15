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

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

data Context = Context
  { hsinspect :: FilePath
  , package_dir :: FilePath
  , ghcflags :: String
  }

data HsInspect m = HsInspect
  { imports :: Context -> FilePath -> m (Either String [Import])
  , index :: Context -> m (Either String [Package])
  }

mkHsInspect :: HsInspect IO
mkHsInspect = HsInspect {..}
  where
    imports :: Context -> FilePath -> IO (Either String [Import])
    imports ctx hs = call ctx ["imports", hs]

    index :: Context -> IO (Either String [Package])
    index ctx = call ctx ["index"]

    call :: FromJSON a => Context -> [String] -> IO (Either String a)
    call Context{hsinspect, package_dir, ghcflags} args = do
      setCurrentDirectory package_dir
      (code, stdout, stderr) <- readProcessWithExitCode hsinspect (args <> ["--json", "--", ghcflags]) ""
      case code of
        ExitFailure i -> pure . Left $ "exit code: " <> show i <> " stderr: " <> stderr
        ExitSuccess -> pure . eitherDecodeStrict $ C.pack stdout

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
