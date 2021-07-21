module Communication.Scripts.MessageHandler.Stream where

import qualified Types.Domain.Socket as Socket
import Data.ByteString.Lazy (ByteString)
import qualified Types.Domain.MessagesHandler as MessagesHandler
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Network.WebSockets as WS

sendMsg :: ByteString -> MessagesHandler.MessagesHandler msg -> IO ()
sendMsg msg handler = do
  print msg
  Socket.sendMsgToScript msg (MessagesHandler.stream handler)

receiveMsg :: MessagesHandler.MessagesHandler msg -> IO ByteString
receiveMsg handler = Socket.getMsgForServer (MessagesHandler.stream handler)

app :: Socket.Stream -> WS.ClientApp ()
app stream conn = do
  _ <- forkIO . forever $ putMsg stream conn
  _ <- forever $ getMsg stream conn
  pure ()
  where
    getMsg :: Socket.Stream -> WS.Connection -> IO ()
    getMsg stream conn = do
      msg <- WS.receiveData conn
      Socket.putMsgForServer msg stream

    putMsg :: Socket.Stream -> WS.Connection -> IO ()
    putMsg stream conn = do
      msg <- Socket.getMsgToScript stream
      WS.sendBinaryData conn msg
