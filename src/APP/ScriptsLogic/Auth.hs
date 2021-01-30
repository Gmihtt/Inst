module APP.ScriptsLogic.Auth where

import           Control.Concurrent  (forkIO)
import qualified APP.ScriptsLogic.Stream as Stream
import qualified APP.ScriptsLogic.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Types.Domain.Scripts.Auth as Auth

execute :: String -> Int -> String -> IO (Stream.Stream Auth.Request Auth.Response)
execute host port path = do
  stream <- Stream.initStream
  forkIO $ WS.runClient host port path (Socket.app stream)
  pure stream
