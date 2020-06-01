{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- based on the haskell-lsp example by Alan Zimmerman
module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.STM
import qualified Data.Cache as C
import Data.Default
import qualified Data.Text as T
import Data.Typeable (typeOf)
import HsInspect.LSP.Impl
import qualified Language.Haskell.LSP.Control as CTRL
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U
import System.Environment (getArgs)
import System.Exit
import qualified System.Log.Logger as L

version :: String
#ifdef CURRENT_PACKAGE_VERSION
version = CURRENT_PACKAGE_VERSION
#else
version = "unknown"
#endif

help :: String
help = "hsinspect-lsp [--help|version]\n"

-- TODO automated integration tests, e.g. using Emacs lsp-mode
main :: IO ()
main = do
  args <- getArgs
  when (elem "--help" args) $
    (putStrLn help) >> exitSuccess
  when (elem "--version" args) $
    (putStrLn version) >> exitSuccess
  res <- run
  case res of
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- TODO replace haskell-lsp (which is huge!) with a minimal jsonrpc
--      implementation that covers only the things we actually support. The
--      advantage would be to speedup installation for the user.
run :: IO Int
run = flip E.catches [E.Handler ioExcept, E.Handler someExcept] $ do
  rin <- atomically newTChan
  let
    dp lf = do
      liftIO $ U.logs "main.run:dp entered"
      _rpid <- forkIO $ reactor lf rin
      liftIO $ U.logs "main.run:dp tchan"
      return Nothing

    callbacks = Core.InitializeCallbacks
      { Core.onInitialConfiguration = const $ Right ()
      , Core.onConfigurationChange = const $ Right ()
      , Core.onStartup = dp
      }

  flip E.finally L.removeAllHandlers $ do
    Core.setupLogger Nothing [] L.DEBUG
    CTRL.run callbacks (lspHandlers rin) lspOptions Nothing

  where
    lspOptions = def
    ioExcept   (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

-- supported requests are duplicated here, in the reactor, and lspHandlers
supported :: [J.ClientMethod]
supported = [J.TextDocumentHover, J.TextDocumentCompletion]

reactor :: Core.LspFuncs () -> TChan FromClientMessage -> IO ()
reactor lf inp = do
  U.logs "reactor:entered"
  caches <- Caches <$> C.newCache Nothing <*> C.newCache Nothing <*> C.newCache Nothing
  let toPos (J.Position line col) = (line + 1, col + 1) -- LSP is zero indexed, ghc is one indexed
      toFile (J.TextDocumentIdentifier doc) = J.uriToFilePath doc
  forever $ do
    inval <- atomically $ readTChan inp
    case inval of

      NotInitialized _ -> do
        U.logs "reactor:init"
        let reg cmd = J.Registration "hsinspect-lsp" cmd Nothing
            regs = J.RegistrationParams (J.List $ reg <$> supported)

        rid <- Core.getNextReqId lf
        Core.sendFunc lf . ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid regs

      ReqHover req@(J.RequestMessage _ _ _ (J.TextDocumentPositionParams (toFile -> Just file) (toPos -> pos) _)) -> do
        U.logs $ "reactor:hover:" ++ show (file, pos)
        res <- runExceptT $ hoverProvider caches file pos
        case res of
          Left err -> do
            U.logs $ "reactor:hover:err:" ++ err
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req Nothing

          Right Nothing -> do
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req Nothing

          Right (Just (Span line' col' line'' col'', txt)) -> do
            let halp = J.Hover
                         (J.HoverContents . J.unmarkedUpContent $ txt)
                         (Just $ J.Range (J.Position line' col') (J.Position line'' col''))
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req (Just halp)

      ReqCompletion req@(J.RequestMessage _ _ _ (J.CompletionParams (toFile -> Just file) (toPos -> pos) _ _)) -> do
        U.logs $ "reactor:complete:" ++ show (file, pos)
        res <- runExceptT $ completionProvider caches file pos
        let none = J.Completions $ J.List []
        case res of
          Left err -> do
            U.logs $ "reactor:complete:err:" ++ err
            Core.sendFunc lf . RspCompletion $ Core.makeResponseMessage req none

          Right symbols -> do
            let render txt = J.CompletionItem txt Nothing (J.List []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            Core.sendFunc lf . RspCompletion $ Core.makeResponseMessage req (J.Completions . J.List $ render <$> symbols)

      -- preemptively populate caches
      NotDidOpenTextDocument (J.NotificationMessage _ _ params) -> do
        U.logs "reactor:open"
        let (J.DidOpenTextDocumentParams (J.TextDocumentItem uri _ _ _)) = params
            Just file = J.uriToFilePath uri
        populated <- runExceptT $ do
          ctx <- cachedContext caches file
          void $ cachedImports caches ctx file
          void $ cachedIndex caches ctx
        case populated of
          Right _ -> pure ()
          Left err ->
            -- TODO cache when we send errors so we don't end up spamming the
            --      user, limit to one popup per package.
            Core.sendFunc lf . NotShowMessage .
              J.NotificationMessage "2.0" J.WindowShowMessage .
                J.ShowMessageParams J.MtWarning $ T.pack err

      -- TODO definitionProvider
      -- TODO signatureHelpProvider
      -- TODO import symbol at point (CodeActionQuickFix?)

      om -> do
        U.logs $ "reactor:HandlerRequest:" ++ (show $ typeOf om)

lspHandlers :: TChan FromClientMessage -> Core.Handlers
lspHandlers rin =
  let passHandler :: (a -> FromClientMessage) -> Core.Handler a
      passHandler c notification = atomically $ writeTChan rin (c notification)
  in def { Core.hoverHandler = Just $ passHandler ReqHover
         , Core.completionHandler = Just $ passHandler ReqCompletion
         , Core.definitionHandler = Just $ passHandler ReqDefinition
         , Core.initializedHandler = Just $ passHandler NotInitialized
         , Core.didOpenTextDocumentNotificationHandler = Just $ passHandler NotDidOpenTextDocument
         -- Emacs lsp-mode sends these, even though we don't ask for them...
         , Core.cancelNotificationHandler = Just $ passHandler NotCancelRequestFromClient
         , Core.responseHandler = Just $ \_ -> pure ()
         }
