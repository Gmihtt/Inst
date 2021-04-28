module Communication.Sockets.Connection
  ( runConnection,
  )
where

import qualified Communication.Sockets.App as App
import qualified Communication.Sockets.Socket as Socket
import qualified Communication.ThreadManager.Manager as Manager
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

type TaskIdGetter = ByteString -> IO Text

type TaskGetter a = ByteString -> Maybe a -> IO a

runConnection ::
  Socket.Socket ->
  TaskIdGetter ->
  TaskGetter a ->
  IO (Manager.Manager a)
runConnection socket getTaskId getTask = do
  manager <- Manager.initManager socket App.run
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
