-- Main.hs, run with ghc test.hs -e main
module Server where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Data (mkIntType)
import Network.Socket
import System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0 -- Create a new socket using address type, socket type, protocol number
  setSocketOption sock ReuseAddr 1 -- ReuseAddr will forcibly bind to existing port in use
  bind sock (SockAddrInet 4242 (0x0100007f)) -- 0x0100007f is 127.0.0.1 in hex using inet_addr
  listen sock 2 -- Listen to a max of 2 connections
  chan <- newChan -- represents a new unbounded channel?
  _ <- forkIO $ fix $ \loop -> do
    -- create a new thread to run the IO computation
    (_, _) <- readChan chan -- reads the next value of the channel
    loop -- loops back to beginning?
  mainLoop sock chan 0 -- call below function with socket, channel

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock -- accept connection, returning (conn, address) pair
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg) -- broadcast message
  hdl <- socketToHandle sock ReadWriteMode -- handle = operations to read and write from files
  hSetBuffering hdl NoBuffering -- buffering disabled on handle if possible
  hPutStrLn hdl "Input Username"
  name <- hGetLine hdl
  broadcast ("--> 000 " ++ name ++ " entered game.<--")
  hPutStrLn hdl ("Welcome, " ++ name ++ "!")

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $ fix $ \loop -> do
    (nextNum, line) <- readChan commLine
    when (msgNum /= nextNum) $ hPutStrLn hdl line
    loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    case line of
      -- If an exception is caught, send a message and break the loop
      "quit" -> hPutStrLn hdl "Bye!"
      -- else, continue looping.
      _ -> broadcast ("--> " ++ line ++ "<--") >> loop -- (name ++ ": " ++ line) >> loop
  killThread reader -- kill after the loop ends
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  hClose hdl -- close the handle