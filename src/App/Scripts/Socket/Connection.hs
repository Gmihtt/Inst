module App.Scripts.Socket.Connection 
  (runConnection,
  ) 
where

import qualified App.Scripts.Socket.API as API
import Common.Error (throwSocketErr)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Socket as Socket

type TaskIdGetter = ByteString -> IO Text
type TaskGetter a = ByteString -> Maybe a -> IO a

runConnection ::
  Socket.Socket ->
  TaskIdGetter ->
  TaskGetter a ->
  IO (Manager.Manager a)
runConnection socket getTaskId getTask = do
  manager <- Manager.initManager socket API.run
  forkIO . forever $ receiver manager getTaskId getTask
  pure manager

receiver ::
  Manager.Manager a ->
  TaskIdGetter ->
  TaskGetter a ->
  IO ()
receiver manager getTaskId getTask = do
  msg <- Manager.receiveMsg manager
  taskId <- getTaskId msg
  oldTask <- Manager.findTask taskId manager
  task <- getTask msg oldTask
  Manager.addTask taskId task manager
  pure ()
