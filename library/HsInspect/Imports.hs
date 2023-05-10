{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Imports
  ( imports,
    Qualified,
  )
where

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import GHC.Types.Target (TargetId(..))
import GHC.Types.Name.Reader (GlobalRdrElt(..), ImpDeclSpec(..), ImportSpec(..), globalRdrEnvElts)
#else
import HscTypes (TargetId(..))
import RdrName (GlobalRdrElt(..), ImpDeclSpec(..), ImportSpec(..), globalRdrEnvElts)
#endif

import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC as GHC
import HsInspect.Sexp
import HsInspect.Util
import HsInspect.Workarounds

imports :: GHC.GhcMonad m => FilePath -> m [Qualified]
imports file = do
  (fromJust -> m, target) <- importsOnly mempty file

  GHC.removeTarget $ TargetModule m
  GHC.addTarget target

  -- performance can be very bad here if the user hasn't compiled recently. We
  -- could do the Index hack and only load things that have .hi files but that
  -- will result in very bizarre behaviour and we don't expect the user's code
  -- to be compilable at this point.
  _ <- GHC.load $ GHC.LoadUpTo m

  rdr_env <- minf_rdr_env' m
  pure . sort $ describe =<< globalRdrEnvElts rdr_env

describe :: GlobalRdrElt -> [Qualified]
describe GRE {gre_name, gre_imp} = describe' <$> gre_imp
  where
    describe' ImpSpec {is_decl = ImpDeclSpec {is_mod, is_as, is_qual}} =
      let ln =
            if is_qual
              then Nothing
              else Just . T.pack $ showGhc gre_name
          lqn =
            if is_mod == is_as
              then Nothing
              else Just . T.pack $ showGhc is_as ++ "." ++ showGhc gre_name
          fqn = T.pack $ showGhc is_mod ++ "." ++ showGhc gre_name
       in Qualified ln lqn fqn

-- 1. local name
-- 2. locally qualified name
-- 3. fully qualified name
data Qualified
  = Qualified
      (Maybe Text)
      (Maybe Text)
      Text
  deriving (Eq, Ord, Show)
{- BOILERPLATE Qualified ToSexp field=[local,qual,full] -}
{- BOILERPLATE START -}
instance ToSexp Qualified where
  toSexp (Qualified p_1_1 p_1_2 p_1_3) = alist [("local", toSexp p_1_1), ("qual", toSexp p_1_2), ("full", toSexp p_1_3)]
{- BOILERPLATE END -}
