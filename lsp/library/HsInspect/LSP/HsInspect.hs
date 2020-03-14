-- Abstraction of the hsinspect binary.
--
-- We intentionally do not depend on the hsinspect library because by decoupling
-- we can use one hsinspect-lsp binary for all projects, with only ghcflags /
-- hsinspect being tied to each project and ghc version.
module HsInspect.LSP.HsInspect where

--import Data.Aeson

data Imports
  = Imports
      (Maybe String)
      (Maybe String)
      String
  deriving (Eq, Show)

-- instance FromJSON Qualified where
--   toSexp (Qualified ln lqn fqn) =
--     alist
--       [ ("local", toSexp ln),
--         ("qual", toSexp lqn),
--         ("full", toSexp fqn)
--       ]
