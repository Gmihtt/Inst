module App.Scripts.Socket.API
  ( run,
  )
where

import qualified App.Scripts.Socket.App as Socket
import Control.Concurrent (forkIO)
import qualified Network.WebSockets as WS
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Stream as Stream

run :: Socket.Socket -> IO Stream.Stream
run socket = do
  let host = Socket.host socket
  let port = Socket.port socket
  let path = Socket.path socket
  stream <- Stream.initStream
  _ <- forkIO $ WS.runClient host port path (Socket.app stream)
  pure stream
