module APP.Scripts.Sockets.API (Stream.Stream, run, sendMsg, receiveMsg) where

import qualified APP.Scripts.Sockets.SocketApp as Socket
import qualified APP.Scripts.Sockets.Stream as Stream
import Control.Concurrent (forkIO)
import Data.ByteString.Lazy (ByteString)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Stream as Stream

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
