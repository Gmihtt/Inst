module Types.Domain.Manager where

import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Communication.Scripts.Auth as Auth
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Stream as Stream

type AuthManager = Manager Auth.Response

type StatisticsManager = Manager Statistic.Statistic

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
  s <- getSocket socket
  m <- Map.empty
  pure $ Manager
    { tasks = m,
      stream = s
    }

sendMsg :: ByteString -> Manager a -> IO ()
sendMsg msg manager = Stream.sendMsgToScript msg (stream manager)

receiveMsg :: Manager a -> IO ByteString
receiveMsg manager = Stream.getMsgForServer (stream manager)