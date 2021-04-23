{-# LANGUAGE OverloadedStrings #-}

module Communication.Sockets.App
  ( run,
  )
where

import qualified Communication.Sockets.Socket as Socket
import qualified Communication.Sockets.Stream as Stream
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Network.WebSockets as WS

run :: Socket.Socket -> IO Stream.Stream
run socket = do
  let host = Socket.host socket
  let port = Socket.port socket
  let path = Socket.path socket
  stream <- Stream.initStream
  _ <- forkIO $ WS.runClient host port path (app stream)
  pure stream

app :: Stream.Stream -> WS.ClientApp ()
app stream conn = do
  _ <- forkIO . forever $ putMsg stream conn
  _ <- forever $ getMsg stream conn
  pure ()

getMsg :: Stream.Stream -> WS.Connection -> IO ()
getMsg stream conn = do
  msg <- WS.receiveData conn
  Stream.putMsgForServer msg stream

putMsg :: Stream.Stream -> WS.Connection -> IO ()
putMsg stream conn = do
  msg <- Stream.getMsgToScript stream
  WS.sendBinaryData conn msg
