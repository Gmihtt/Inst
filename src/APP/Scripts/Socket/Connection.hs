module APP.Scripts.Socket.Connection (runConnection) where

import qualified APP.Scripts.Socket.API as API
import Common.Error (throwSocketErr)
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Map as Map
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Scripts.Auth as Auth
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Stream as Stream

runConnection ::
  Socket.Socket ->
  (ByteString -> IO Text) ->
  (ByteString -> Maybe a -> IO a) ->
  IO (Manager.Manager a)
runConnection socket getKey getValue = do
  stream <- API.run socket
  manager <- Map.empty
  let threadsMap =
        Manager.Manager
          { Manager.manager = manager,
            Manager.stream = stream
          }
  forkIO . forever $ receiver threadsMap getKey getValue
  pure threadsMap

receiver ::
  Manager.Manager a ->
  (ByteString -> IO Text) ->
  (ByteString -> Maybe a -> IO a) ->
  IO ()
receiver (Manager.Manager manager stream) getKey getValue = do
  msg <- API.receiveMsg stream
  key <- getKey msg
  oldValue <- Map.lookup key manager
  value <- getValue msg oldValue
  Map.insert key value manager
  pure ()
