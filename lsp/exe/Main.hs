{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import HsInspect.LSP.Context (BuildTool(..))
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
help = "hsinspect-lsp [--help|version|stack]\n"

-- TODO automated integration tests, e.g. using Emacs lsp-mode
main :: IO ()
main = do
  args <- getArgs
  when (elem "--help" args) $
    (putStrLn help) >> exitSuccess
  when (elem "--version" args) $
    (putStrLn version) >> exitSuccess
  let tool = if (elem "--stack" args) then Stack else Cabal
  res <- run tool
  case res of
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- TODO replace haskell-lsp (which is huge!) with a minimal jsonrpc
--      implementation that covers only the things we actually support. The
--      advantage would be to speedup installation for the user.
run :: BuildTool -> IO Int
run tool = flip E.catches [E.Handler ioExcept, E.Handler someExcept] $ do
  rin <- atomically newTChan
  let
    dp lf = do
      liftIO $ U.logs "main.run:dp entered"
      _rpid <- forkIO $ reactor tool lf rin
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
supported = [J.TextDocumentHover]

reactor :: BuildTool -> Core.LspFuncs () -> TChan FromClientMessage -> IO ()
reactor tool lf inp = do
  U.logs "reactor:entered"
  caches <- Caches <$> C.newCache Nothing <*> C.newCache Nothing <*> C.newCache Nothing
  forever $ do
    inval <- atomically $ readTChan inp
    case inval of

      NotInitialized _notification -> do
        U.logs "reactor:init"
        let reg cmd = J.Registration "hsinspect-lsp" cmd Nothing
            regs = J.RegistrationParams (J.List $ reg <$> supported)

        rid <- Core.getNextReqId lf
        Core.sendFunc lf . ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid regs

      (ReqHover req@(J.RequestMessage _ _ _ params)) -> do
        U.logs $ "reactor:hover:" ++ show params
        let J.TextDocumentPositionParams (J.TextDocumentIdentifier doc) (J.Position line col) _ = params
            Just file = J.uriToFilePath doc
        res <- runExceptT $ hoverProvider caches tool file (line + 1, col + 1)
        case res of
          Left err -> do
            U.logs $ "reactor:hover:err:" ++ err
            -- the only way to get a popup on the user's screen is to use a show
            -- notification, the ErrorReq ends up being rendered exactly the
            -- same as a success, so useless.
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req Nothing
            Core.sendFunc lf . NotShowMessage $
              J.NotificationMessage "2.0" J.WindowShowMessage (J.ShowMessageParams J.MtWarning $ T.pack err)

          Right Nothing -> do
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req Nothing

          Right (Just (Span line' col' line'' col'', txt)) -> do
            let halp = J.Hover
                         (J.HoverContents . J.unmarkedUpContent $ txt)
                         (Just $ J.Range (J.Position line' col') (J.Position line'' col''))
            Core.sendFunc lf . RspHover $ Core.makeResponseMessage req (Just halp)

      -- preemptively populate caches
      NotDidOpenTextDocument (J.NotificationMessage _ _ params) -> do
        U.logs "reactor:open"
        let (J.DidOpenTextDocumentParams (J.TextDocumentItem uri _ _ _)) = params
            Just file = J.uriToFilePath uri
        -- TODO forkIO
        void . runExceptT $ do
          ctx <- cachedContext caches tool file
          void $ cachedImports caches ctx file
          void $ cachedIndex caches ctx

      -- TODO completionProvider
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
