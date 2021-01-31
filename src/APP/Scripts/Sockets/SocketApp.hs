{-# LANGUAGE OverloadedStrings #-}

module APP.Scripts.Sockets.SocketApp (app) where

import           Control.Concurrent  (forkIO, ThreadId)
import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import Data.Text
import Common.Error (throwSocketErr)
import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import qualified Types.Domain.Stream as Stream
import qualified APP.Scripts.Sockets.Stream as Stream
import qualified Types.Domain.Scripts.Auth as Auth

app :: Stream.Stream -> WS.ClientApp ()
app stream conn = do
  _ <- forkIO . forever $ putMsg stream conn
  _ <- forever $ getMsg stream conn
  pure ()

getMsg :: Stream.Stream -> WS.Connection -> IO ()
getMsg stream conn = do
  msg <- WS.receiveData conn
  Stream.putMsgForServer msg stream

putMsg ::Stream.Stream -> WS.Connection -> IO ()
putMsg stream conn = do
  msg <- Stream.getMsgToScript stream
  print "awdawd"
  print msg
  WS.sendBinaryData conn msg