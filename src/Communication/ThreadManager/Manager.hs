{-# LANGUAGE RecordWildCards #-}

module Communication.ThreadManager.Manager where

import qualified Communication.Sockets.Socket as Socket
import qualified Communication.Sockets.Stream as Stream
import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

data Manager a
  = Manager
      { tasks :: Map.Map Text a,
        stream :: Stream.Stream
      }

addTask :: Text -> a -> Manager a -> IO Bool
addTask taskId value manager = Map.insert taskId value (tasks manager)

findTask :: Text -> Manager a -> IO (Maybe a)
findTask taskId manager = Map.lookup taskId (tasks manager)

deleteTask :: Text -> Manager a -> IO Bool
deleteTask taskId manager = Map.delete taskId (tasks manager)

initManager :: Socket.Socket -> (Socket.Socket -> IO Stream.Stream) -> IO (Manager a)
initManager socket getSocket = do
  stream <- getSocket socket
  tasks <- Map.empty
  pure Manager {..}

sendMsg :: ByteString -> Manager a -> IO ()
sendMsg msg manager = do
  print msg
  Stream.sendMsgToScript msg (stream manager)

receiveMsg :: Manager a -> IO ByteString
receiveMsg manager = Stream.getMsgForServer (stream manager)
