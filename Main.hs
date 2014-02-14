module Main where

import Control.Concurrent
import Control.Exception
import Data.Time
import Network
import System.Console.ANSI

-- | Configuration

port :: PortNumber
port = 8666

-- | Main

main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber port)
    handleConnections socket
    
handleConnections :: Socket -> IO ()
handleConnections socket = do
    (handle, hostName, portNumber) <- accept socket
    message Info ("Incoming connection from " ++ hostName ++ ":" ++ show portNumber)
    threadId <- forkFinally handleRequest handleException
    message Info ("Forked a thread with id " ++ show threadId)
    handleConnections socket
    
handleRequest :: IO ()
handleRequest = undefined

handleException :: Show a => Either SomeException a -> IO ()
handleException (Left someException) = message Warning $
    "A thread terminated with the exception: " ++ show someException
handleException (Right x) = message Info $
    "A thread terminated gracefully with value: " ++ show x

-- | Logging

data LogClass = Info | Warning

logColor :: LogClass -> Color
logColor Info    = Blue
logColor Warning = Red

message :: LogClass -> String -> IO ()
message logClass msg = do
    currentTime <- getCurrentTime
    setSGR [SetColor Foreground Vivid Black]
    putStr ("[" ++ show currentTime ++ "] ")
    setSGR [SetColor Foreground Dull (logColor logClass)]
    putStrLn msg
