{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Socket.App
  ( app,
  )
where

import Common.Error (throwSocketErr)
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import qualified Types.Communication.Scripts.Auth as Auth
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