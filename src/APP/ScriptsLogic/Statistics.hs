module APP.ScriptsLogic.Statistics where

import           Control.Concurrent  (forkIO)
import qualified APP.ScriptsLogic.Stream as Stream
import qualified APP.ScriptsLogic.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Types.Domain.Scripts.Statistics as Statistics

execute :: String -> Int -> String -> IO (Stream.Stream Statistics.Request Statistics.Response)
execute host port path = do
  stream <- Stream.initStream
  forkIO $ WS.runClient host port path (Socket.app stream)
  pure stream
