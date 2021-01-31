module APP.Scripts.Sockets.API (Stream.Stream, run, sendMsg, receiveMsg) where

import           Control.Concurrent  (forkIO)
import Data.ByteString.Lazy (ByteString)
import qualified APP.Scripts.Sockets.Stream as Stream
import qualified Types.Domain.Stream as Stream
import qualified APP.Scripts.Sockets.SocketApp as Socket
import           Network.Socket      (withSocketsDo)
import qualified Types.Domain.Socket as Socket
import qualified Network.WebSockets as WS

run :: Socket.Socket -> IO Stream.Stream
run socket = do
  let host = Socket.host socket 
  let port = Socket.port socket 
  let path = Socket.path socket 
  stream <- Stream.initStream
  forkIO $ WS.runClient host port path (Socket.app stream)
  pure stream

sendMsg :: ByteString -> Stream.Stream -> IO ()
sendMsg = Stream.sendMsgToScript

receiveMsg :: Stream.Stream -> IO ByteString
receiveMsg = Stream.getMsgForServer