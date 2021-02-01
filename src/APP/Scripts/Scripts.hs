module APP.Scripts.Scripts
  ( runConnection,
    sendAndReceiveMsg,
  )
where

import qualified APP.Scripts.Sockets.API as API
import Common.Error (throwSocketErr, throwThreadsError)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Map as Map
import Control.Monad (forever, unless)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Threads as Threads

runConnection :: Socket.Socket -> (ByteString -> IO Text) -> IO Threads.ThreadsMap
runConnection socket getKey = do
  stream <- API.run socket
  threads <- Map.empty
  let threadsMap =
        Threads.ThreadsMap
          { Threads.threads = threads,
            Threads.stream = stream
          }
  receiver threadsMap getKey
  pure threadsMap

receiver :: Threads.ThreadsMap -> (ByteString -> IO Text) -> IO ThreadId
receiver (Threads.ThreadsMap threads stream) getKey = forkIO . forever $ do
  msg <- API.receiveMsg stream
  key <- getKey msg
  Map.insert key msg threads

sendAndReceiveMsg :: (FromJSON a, FromJSON b, ToJSON a, ToJSON b) => Text -> Threads.ThreadsMap -> a -> IO b
sendAndReceiveMsg key (Threads.ThreadsMap threads stream) req = do
  API.sendMsg (encode req) stream
  script <- newEmptyMVar
  getMsg script
  bsRes <- takeMVar script
  Map.delete key threads
  maybe (throwSocketErr $ "decode fail" <> show bsRes) pure (decode bsRes)
  where
    getMsg script = do
      sleepSecond
      mbMsg <- Map.lookup key threads
      maybe (getMsg script) (putMVar script) mbMsg
    sleepSecond = threadDelay 1000000
