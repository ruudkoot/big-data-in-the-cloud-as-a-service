module Main where

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Reader
import Network

import Handler
import Logging

-- | Configuration

port :: PortNumber
port = 8666

-- | Main

main :: IO ()
main = withSocketsDo $ do
    chan <- newChan
    forkIO (messageProcess chan)
    socket <- listenOn (PortNumber port)
    runReaderT (handleConnections socket) chan

-- | Connection handling

handleConnections :: Socket -> Handler ()
handleConnections socket = do

    (handle, hostName, portNumber) <- liftIO $ accept socket

    message Info $
        "Incoming connection from " ++ hostName ++ ":" ++ show portNumber

    handleRequest'   <- runHandler  handleRequest
    handleException' <- runHandler1 handleException
    threadId         <- liftIO $ forkFinally handleRequest' handleException'

    message Info $
        "Forked a thread with id " ++ show threadId

    handleConnections socket
    
handleRequest :: Handler ()
handleRequest = message Warning $
    "handleRequest"

handleException :: Show a => Either SomeException a -> Handler ()
handleException (Left someException) = message Warning $
    "A thread terminated with the exception: " ++ show someException
handleException (Right x) = message Info $
    "A thread terminated gracefully with value: " ++ show x
