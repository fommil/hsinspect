{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Imports
  ( imports,
    Qualified,
  )
where

#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
import qualified GHC.Types.Target as GHC
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
import qualified GHC.Driver.Env.Types as GHC
import qualified GHC.Unit.Env as GHC
import qualified GHC.Data.Bag as GHC
import qualified GHC.Types.Name.Reader as GHC
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Types.Name.Reader as GHC
#else
import qualified HscTypes as GHC
import qualified RdrName as GHC
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

  GHC.removeTarget $ GHC.TargetModule m
  GHC.addTarget target

  -- performance can be very bad here if the user hasn't compiled recently. We
  -- could do the Index hack and only load things that have .hi files but that
  -- will result in very bizarre behaviour and we don't expect the user's code
  -- to be compilable at this point.
#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
  sess <- GHC.getSession
  let unitid = GHC.ue_current_unit $ GHC.hsc_unit_env sess
  _ <- GHC.load $ GHC.LoadUpTo (GHC.mkModule unitid m)
#else
  _ <- GHC.load $ GHC.LoadUpTo m
#endif

  rdr_env <- minf_rdr_env' m
  pure . sort $ describe =<< GHC.globalRdrEnvElts rdr_env

describe :: GHC.GlobalRdrElt -> [Qualified]
describe GHC.GRE {GHC.gre_name, GHC.gre_imp} =
#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
  describe' <$> GHC.bagToList gre_imp
#else
  describe' <$> gre_imp
#endif
  where
    describe' GHC.ImpSpec {GHC.is_decl = GHC.ImpDeclSpec {GHC.is_mod, GHC.is_as, GHC.is_qual}} =
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
