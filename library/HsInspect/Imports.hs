{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Imports
  ( imports,
    Qualified,
  )
where

import Data.Maybe (fromJust)
import DynFlags (unsafeGlobalDynFlags)
import qualified GHC as GHC
import HsInspect.Sexp
import HsInspect.Workarounds
import HscTypes (TargetId (..))
import Json
import Outputable (Outputable, showPpr)
import RdrName
  ( GlobalRdrElt (..),
    ImpDeclSpec (..),
    ImportSpec (..),
    globalRdrEnvElts,
  )

imports :: GHC.GhcMonad m => FilePath -> m [Qualified]
imports file = do
  gres <- imports' file
  pure $ describe =<< gres

imports' :: GHC.GhcMonad m => FilePath -> m [GlobalRdrElt]
imports' file = do
  -- TODO no need for this in 8.8.2+
  (fromJust -> m, target) <- importsOnly mempty file

  GHC.removeTarget $ TargetModule m
  GHC.addTarget target

  -- TODO performance can be very bad here. It is possible that ghc is compiling
  -- modules in the home module that have .hi files that would be much faster.
  _ <- GHC.load $ GHC.LoadUpTo m

  rdr_env <- minf_rdr_env' m
  pure $ globalRdrEnvElts rdr_env

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

describe :: GlobalRdrElt -> [Qualified]
describe GRE {gre_name, gre_imp} = describe' <$> gre_imp
  where
    describe' ImpSpec {is_decl = ImpDeclSpec {is_mod, is_as, is_qual}} =
      let ln =
            if is_qual
              then Nothing
              else Just $ showGhc gre_name
          lqn =
            if is_mod == is_as
              then Nothing
              else Just $ showGhc is_as ++ "." ++ showGhc gre_name
          fqn = showGhc is_mod ++ "." ++ showGhc gre_name
       in Qualified ln lqn fqn

-- Note that `nameSrcLoc gre_name` is empty
-- TODO unitid (can be used to lookup source code)
-- TODO "and originally defined" / ppr_defn_site

-- 1. local name
-- 2. locally qualified name
-- 3. fully qualified name
data Qualified
  = Qualified
      (Maybe String)
      (Maybe String)
      String
  deriving (Eq, Show)

instance ToSexp Qualified where
  toSexp (Qualified ln lqn fqn) =
    alist
      [ ("local", toSexp ln),
        ("qual", toSexp lqn),
        ("full", toSexp fqn)
      ]

instance ToJson Qualified where
  json (Qualified ln lqn fqn) =
    JSObject
      [ ("local", json' ln),
        ("qual", json' lqn),
        ("full", JSString fqn)
      ]
    where
      json' Nothing = JSNull
      json' (Just a) = JSString a
