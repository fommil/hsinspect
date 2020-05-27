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

reactor :: BuildTool -> Core.LspFuncs () -> TChan FromClientMessage -> IO ()
reactor tool lf inp = do
  U.logs "reactor:entered"
  caches <- Caches <$> C.newCache Nothing <*> C.newCache Nothing
  forever $ do
    inval <- atomically $ readTChan inp
    case inval of

      (ReqSignatureHelp req@(J.RequestMessage _ _ _ params)) -> do
        U.logs $ "reactor:SignatureHelp:" ++ show params
        let J.TextDocumentPositionParams (J.TextDocumentIdentifier doc) (J.Position line col) _ = params
            Just file = J.uriToFilePath doc
        res <- runExceptT $ signatureHelpProvider caches tool file (line + 1, col + 1)
        case res of
          Left err -> do
            U.logs $ "reactor:signatureHelpProvider:err:" ++ err
            -- FIXME how to send an error to the client?
          Right sigs -> do
            let render s = J.SignatureInformation s Nothing Nothing
                halp = J.SignatureHelp (J.List $ render <$> sigs) Nothing Nothing
            Core.sendFunc lf . RspSignatureHelp $ Core.makeResponseMessage req halp

      -- TODO completionProvider
      -- TODO definitionProvider

      -- TODO DidOpenTextDocument is a good opportunity to preemptively populate caches

      -- TODO hygienic registration of supported commands

      om -> do
        U.logs $ "reactor:HandlerRequest:" ++ (show $ typeOf om)

lspHandlers :: TChan FromClientMessage -> Core.Handlers
lspHandlers rin =
  let passHandler :: (a -> FromClientMessage) -> Core.Handler a
      passHandler c notification = atomically $ writeTChan rin (c notification)
  in def { Core.signatureHelpHandler = Just $ passHandler ReqSignatureHelp
         , Core.completionHandler = Just $ passHandler ReqCompletion
         , Core.definitionHandler = Just $ passHandler ReqDefinition
         , Core.initializedHandler = Just $ passHandler NotInitialized
         , Core.cancelNotificationHandler = Just $ passHandler NotCancelRequestFromClient
         , Core.didOpenTextDocumentNotificationHandler = Just $ passHandler NotDidOpenTextDocument
         }
