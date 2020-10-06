{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HsInspect.Types where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified DynFlags as GHC
import GHC (HscEnv)
import qualified GHC as GHC
import HsInspect.Sexp
import qualified HsInspect.Util as H
import HsInspect.Workarounds (mkCppState)
import qualified Lexer as GHC
import qualified Outputable as GHC
import qualified Parser

data Type = ProductType Text [Text] Text [Text]  -- ^^ type tparams cons [param types]
          | RecordType Text [Text] Text [(Text, Text)] -- ^^ type tparams cons [(fieldname, param type)]
          | SumType Text [Text] [(Text, [Text])] -- ^^ type tparams [(cons, param types)] (no records)
  deriving (Eq, Show)
{- BOILERPLATE Type ToSexp
   field={ProductType:[type,tparams,cons,params],
          RecordType:[type,tparams,cons,fields],
          SumType:[type,tparams,data]}
   class={ProductType:product,
          RecordType:record,
          SumType:sum}
-}
{- BOILERPLATE START -}
instance ToSexp Type where
  toSexp (ProductType p_1_1 p_1_2 p_1_3 p_1_4) = alist $ ("class", "product") : [("type", toSexp p_1_1), ("tparams", toSexp p_1_2), ("cons", toSexp p_1_3), ("params", toSexp p_1_4)]
  toSexp (RecordType p_1_1 p_1_2 p_1_3 p_1_4) = alist $ ("class", "record") : [("type", toSexp p_1_1), ("tparams", toSexp p_1_2), ("cons", toSexp p_1_3), ("fields", toSexp p_1_4)]
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

-- FIXME add a Bool to each field to indicate if it is polymorphic

types :: GHC.GhcMonad m => FilePath -> m ([Type], [Comment])
types file = do
  dflags <- GHC.getSessionDynFlags
  _ <- GHC.setSessionDynFlags $ GHC.gopt_set dflags GHC.Opt_KeepRawTokenStream
  env <- GHC.getSession
  liftIO $ parseTypes env file

parseTypes :: HscEnv -> FilePath -> IO ([Type], [Comment])
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
          findType (GHC.L _ (GHC.TyClD _ (GHC.DataDecl _ tycon' (GHC.HsQTvs _ tparams') _ ddn))) =
            let
              tycon = showGhc tycon'
              tparams = renderTparam <$> tparams'
              renderField :: GHC.GenLocated l (GHC.ConDeclField GHC.GhcPs) -> (Text, Text)
              renderField (GHC.L _ field) = (showGhc . head $ GHC.cd_fld_names field, showGhc $ GHC.cd_fld_type field)
              renderArg :: GHC.LBangType GHC.GhcPs -> Text
              renderArg (GHC.L _ arg) = showGhc arg
              rhs = do
                (GHC.L _ ddl) <- GHC.dd_cons ddn
                case ddl of
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.RecCon (GHC.L _ fields)) _ -> [(showGhc cons, Left $ renderField <$> fields)]
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.InfixCon a1 a2) _ -> [(showGhc cons, Right $ [renderArg a1, renderArg a2])]
                  GHC.ConDeclH98 _ cons _ _ _ (GHC.PrefixCon args) _ -> [(showGhc cons, Right $ renderArg <$> args)]
                  _ -> [] -- GADTS

             in case rhs of
              [] -> Nothing
              [(cons, Right tpes)] -> Just $ ProductType tycon tparams cons tpes
              [(cons, Left fields)] -> Just $ RecordType tycon tparams cons fields
              mult -> Just . SumType tycon tparams $ render <$> mult
                where
                  render (cons, Right args) = (cons, args)
                  render (cons, Left fargs) = (cons, snd <$> fargs)

          findType _ = Nothing

          renderTparam :: GHC.GenLocated l (GHC.HsTyVarBndr GHC.GhcPs) -> Text
          renderTparam (GHC.L _ (GHC.UserTyVar _ p)) = showGhc p
          renderTparam (GHC.L _ (GHC.KindedTyVar _ p _)) = showGhc p
          renderTparam (GHC.L _ (GHC.XTyVarBndr _)) = "<unsupported>"

          extractComment (GHC.L (GHC.RealSrcSpan pos) c) =
            let start = Pos (GHC.srcSpanStartLine pos) (GHC.srcSpanStartCol pos)
                end = Pos (GHC.srcSpanEndLine pos) (GHC.srcSpanEndCol pos)
             in (\str -> Comment (T.pack str) start end) <$> case c of
            (GHC.AnnLineComment txt) -> Just txt
            (GHC.AnnBlockComment txt) -> Just txt
            _ -> Nothing
          extractComment _ = Nothing

          types = mapMaybe findType decls
          comments = mapMaybe extractComment $ GHC.comment_q st

      pure (types, sortOn (\(Comment _ s _) -> s) comments)

    GHC.PFailed _ _ err -> throwIO . userError $ "unable to parse " <> file <> " due to " <> GHC.showSDocUnsafe err
