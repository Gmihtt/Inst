module Communication.Scripts.MessagesHandler.Stream where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import qualified Types.Domain.MessagesHandler as MessagesHandler
import qualified Types.Domain.Socket as Socket

sendMsg :: ByteString -> MessagesHandler.MessagesHandler key msg -> IO ()
sendMsg msg handler = do
  print msg
  Socket.sendMsgToScript msg (MessagesHandler.stream handler)

receiveMsg :: MessagesHandler.MessagesHandler key msg -> IO ByteString
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
