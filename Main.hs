module Main where

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Reader
import System.IO
import Network

import Handler
import Logging
import HTTP

-- | Configuration

port :: PortID
port = PortNumber 8666

-- | Main

main :: IO ()
main = withSocketsDo $ do
    chan <- newChan
    forkIO (messageProcess chan)
    socket <- listenOn port
    runReaderT (handleConnections socket) chan

-- | Connection handling

handleConnections :: Socket -> Handler ()
handleConnections socket = do

    (handle, hostName, portNumber) <- liftIO $ accept socket

    message Info $
        "Incoming connection from " ++ hostName ++ ":" ++ show portNumber

    handleRequest'   <- runHandler  (handleRequest handle)
    handleException' <- runHandler1 handleException
    threadId         <- liftIO $ forkFinally handleRequest' handleException'

    message Info $
        "Forked a thread with id " ++ show threadId

    handleConnections socket
    
handleRequest :: Handle -> Handler ()
handleRequest handle = do
    message Warning $ "handleRequest"
    contents <- liftIO $ hGetContents handle
    let request@(Request _ url _ _) = parseRequest contents
    message' Info request
    liftIO $ do
        hPutStrLn handle . show $ respond200 $ "This is " ++ url ++ "!"
        hClose handle

handleException :: Show a => Either SomeException a -> Handler ()
handleException (Left someException) = message Warning $
    "A thread terminated with the exception: " ++ show someException
handleException (Right x) = message Info $
    "A thread terminated gracefully with value: " ++ show x
