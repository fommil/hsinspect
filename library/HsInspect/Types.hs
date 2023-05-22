{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module HsInspect.Types where

#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
import qualified GHC.Utils.Error as GHC
import qualified GHC.Types.Error as GHC
#elif MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
import qualified GHC.Utils.Error as GHC
import qualified GHC.Parser.Errors.Ppr as GHC
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Utils.Error as GHC
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
import qualified ErrUtils as GHC
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser as Parser
import qualified GHC.Rename.HsType as GHC
#else
import qualified DynFlags as GHC
import qualified Lexer as GHC
import qualified Outputable as GHC
import qualified Parser as Parser
import qualified RnTypes as GHC
#endif

import qualified GHC as GHC

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import HsInspect.Sexp
import qualified HsInspect.Util as H
import HsInspect.Workarounds (mkCppState)

data Type = ProductType Text [Text] Bool Text [(Text, [Text])]  -- ^^ type tparams newtype cons [(param types, [typarams])]
          | RecordType Text [Text] Bool Text [(Text, Text, [Text])] -- ^^ type tparams newtype cons [(fieldname, param type, [typarams])]
          | SumType Text [Text] [(Text, [(Text, [Text])])] -- ^^ type tparams [(cons, [param types, [typarams]])] (no records)
  deriving (Eq, Show)
{- BOILERPLATE Type ToSexp
   field={ProductType:[type,tparams,newtype,cons,params],
          RecordType:[type,tparams,newtype,cons,fields],
          SumType:[type,tparams,data]}
   class={ProductType:product,
          RecordType:record,
          SumType:sum}
-}
{- BOILERPLATE START -}
instance ToSexp Type where
  toSexp (ProductType p_1_1 p_1_2 p_1_3 p_1_4 p_1_5) = alist $ ("class", "product") : [("type", toSexp p_1_1), ("tparams", toSexp p_1_2), ("newtype", toSexp p_1_3), ("cons", toSexp p_1_4), ("params", toSexp p_1_5)]
  toSexp (RecordType p_1_1 p_1_2 p_1_3 p_1_4 p_1_5) = alist $ ("class", "record") : [("type", toSexp p_1_1), ("tparams", toSexp p_1_2), ("newtype", toSexp p_1_3), ("cons", toSexp p_1_4), ("fields", toSexp p_1_5)]
  toSexp (SumType p_1_1 p_1_2 p_1_3) = alist $ ("class", "sum") : [("type", toSexp p_1_1), ("tparams", toSexp p_1_2), ("data", toSexp p_1_3)]
{- BOILERPLATE END -}

-- line, col (1-indexed)
data Pos = Pos Int Int
  deriving (Eq, Ord, Show)
{- BOILERPLATE Pos ToSexp field=[line,col] -}
{- BOILERPLATE START -}
instance ToSexp Pos where
  toSexp (Pos p_1_1 p_1_2) = alist [("line", toSexp p_1_1), ("col", toSexp p_1_2)]
{- BOILERPLATE END -}

data Comment = Comment Text Pos Pos -- text start end
  deriving (Eq, Show)
{- BOILERPLATE Comment ToSexp field=[text,start,end] -}
{- BOILERPLATE START -}
instance ToSexp Comment where
  toSexp (Comment p_1_1 p_1_2 p_1_3) = alist [("text", toSexp p_1_1), ("start", toSexp p_1_2), ("end", toSexp p_1_3)]
{- BOILERPLATE END -}

types :: GHC.GhcMonad m => FilePath -> m ([Type], [Comment])
types file = do
  dflags <- GHC.getSessionDynFlags
  _ <- GHC.setSessionDynFlags $ GHC.gopt_set dflags GHC.Opt_KeepRawTokenStream
  env <- GHC.getSession
  liftIO $ parseTypes env file

parseTypes :: GHC.HscEnv -> FilePath -> IO ([Type], [Comment])
parseTypes env file = do
  (pstate, _) <- mkCppState env file
  let showGhc :: GHC.Outputable a => a -> Text
      showGhc = T.pack . H.showGhc
  case GHC.unP Parser.parseModule pstate of
    -- ParseResult (Located (HsModule GhcPs))
    GHC.POk st (GHC.L _ hsmod) -> do
      -- http://hackage.haskell.org/package/ghc-8.8.3/docs/HsDecls.html#t:HsDecl
      -- [Located (HsDecl p)]
      let decls = GHC.hsmodDecls hsmod
          findType (GHC.L _ (GHC.TyClD _ (GHC.DataDecl _ tycon' (GHC.HsQTvs _ tparams') fixity ddn))) =
            let
              tycon = case fixity of
                GHC.Prefix -> showGhc tycon'
                GHC.Infix -> "(" <> showGhc tycon' <> ")"
              tparams = renderTparam <$> tparams'
#if MIN_VERSION_GLASGOW_HASKELL(9,5,0,0)
              nt = case GHC.dd_cons ddn of
                GHC.NewTypeCon _ -> True
                GHC.DataTypeCons _ _ -> False
#else
              nt = case GHC.dd_ND ddn of
                GHC.NewType -> True
                GHC.DataType -> False
#endif
              renderTyParams :: GHC.LHsType GHC.GhcPs -> [Text]
              renderTyParams tpe = showGhc <$>
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
                GHC.extractHsTyRdrTyVars tpe
#else
                (GHC.freeKiTyVarsTypeVars $ GHC.extractHsTyRdrTyVars tpe)
#endif
              renderField :: GHC.GenLocated l (GHC.ConDeclField GHC.GhcPs) -> (Text, Text, [Text]) -- (name, type, [typarams])
              renderField (GHC.L _ field) =
                let tpe = GHC.cd_fld_type field
                 in (showGhc . head $ GHC.cd_fld_names field, showGhc tpe, renderTyParams tpe)
              renderArg' :: GHC.LBangType GHC.GhcPs -> (Text, [Text]) -- (type, typarams)
              renderArg' t@(GHC.L _ arg) = (showGhc arg, renderTyParams t)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
              renderArg = renderArg' . GHC.hsScaledThing
#else
              renderArg = renderArg'
#endif
              -- rhs is (cons, [(field name, field type, [typarams])] | [(parameter type, [typarams])])
              rhs = do
                (GHC.L _ ddl) <- GHC.dd_cons ddn
                case ddl of
                  -- http://hackage.haskell.org/package/ghc-8.8.3/docs/HsDecls.html#t:ConDecl
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.RecCon (GHC.L _ fields)) _ -> [(showGhc cons, Left $ renderField <$> fields)]
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.InfixCon a1 a2) _ -> [("(" <> showGhc cons <> ")", Right $ renderArg <$> [a1, a2])]
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.PrefixCon _ args) _ -> [(showGhc cons, Right $ renderArg <$> args)]
#else
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.PrefixCon args) _ -> [(showGhc cons, Right $ renderArg <$> args)]
#endif
                  _ -> [] -- GADTS

             in case rhs of
              [] -> Nothing
              [(cons, Right tpes)] -> Just $ ProductType tycon tparams nt cons tpes
              [(cons, Left fields)] -> Just $ RecordType tycon tparams nt cons fields
              mult -> Just . SumType tycon tparams $ render <$> mult
                where
                  render (cons, Right args) = (cons, args)
                  render (cons, Left fargs) = (cons, (\(_, tpes, typs) -> (tpes, typs)) <$> fargs)

          findType _ = Nothing

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
          renderTparam :: GHC.LHsTyVarBndr () GHC.GhcPs -> Text
          renderTparam (GHC.L _ (GHC.UserTyVar _ _ p)) = showGhc p
          renderTparam (GHC.L _ (GHC.KindedTyVar _ _ p _)) = showGhc p
#else
          renderTparam :: GHC.GenLocated l (GHC.HsTyVarBndr GHC.GhcPs) -> Text
          renderTparam (GHC.L _ (GHC.UserTyVar _ p)) = showGhc p
          renderTparam (GHC.L _ (GHC.KindedTyVar _ p _)) = showGhc p
          renderTparam (GHC.L _ (GHC.XTyVarBndr _)) = "<unsupported>"
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
          extractComment (GHC.L (GHC.anchor -> pos) c) =
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
          extractComment (GHC.L pos c) =
#else
          extractComment (GHC.L (GHC.RealSrcSpan pos) c) =
#endif
            let start = Pos (GHC.srcSpanStartLine pos) (GHC.srcSpanStartCol pos)
                end = Pos (GHC.srcSpanEndLine pos) (GHC.srcSpanEndCol pos)
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
             in (\str -> Comment (T.pack str) start end) <$> case GHC.ac_tok c of
            (GHC.EpaLineComment txt) -> Just txt
            (GHC.EpaBlockComment txt) -> Just txt
            _ -> Nothing
#else
             in (\str -> Comment (T.pack str) start end) <$> case c of
            (GHC.AnnLineComment txt) -> Just txt
            (GHC.AnnBlockComment txt) -> Just txt
            _ -> Nothing
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
#else
          extractComment _ = Nothing
#endif
          types = mapMaybe findType decls
          comments = mapMaybe extractComment $ GHC.comment_q st

      pure (types, sortOn (\(Comment _ s _) -> s) comments)

#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
    GHC.PFailed st ->
      let errs = GHC.interppSP
            . GHC.pprMsgEnvelopeBagWithLoc
            . GHC.getMessages
            $ GHC.getPsErrorMessages st
      in throwIO . userError $ "unable to parse " <> file <> " due to " <> GHC.showSDocUnsafe errs
#elif MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
    GHC.PFailed st ->
      let errs = GHC.interppSP
            . GHC.pprMsgEnvelopeBagWithLoc
            . fmap GHC.pprError
            $ GHC.getErrorMessages st
      in throwIO . userError $ "unable to parse " <> file <> " due to " <> GHC.showSDocUnsafe errs
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    GHC.PFailed st ->
      let errs = GHC.interppSP
            . GHC.pprErrMsgBagWithLoc
            . GHC.getErrorMessages st
            $ GHC.unsafeGlobalDynFlags
      in throwIO . userError $ "unable to parse " <> file <> " due to " <> GHC.showSDocUnsafe errs
#else
    GHC.PFailed _ _ err -> throwIO . userError $ "unable to parse " <> file <> " due to " <> GHC.showSDocUnsafe err
#endif
