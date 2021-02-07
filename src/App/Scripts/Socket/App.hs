{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Socket.App
  ( app,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Network.WebSockets as WS
import qualified Types.Domain.Stream as Stream

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
