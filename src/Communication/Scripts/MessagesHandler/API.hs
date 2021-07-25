module Communication.Scripts.MessagesHandler.API
  ( module Stream,
    module Messages,
    module MessagesHandler,
    runMessagesHandler,
  )
where

import Communication.Scripts.MessagesHandler.Messages as Messages
  ( addMsg,
    deleteMsg,
    findMsg,
  )
import Communication.Scripts.MessagesHandler.Stream as Stream
  ( app,
    receiveMsg,
    sendMsg,
  )
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Hashable (Hashable)
import qualified Network.WebSockets as WS
import Types.Domain.MessagesHandler as MessagesHandler
import qualified Types.Domain.Socket as Socket

runMessagesHandler ::
  (Eq key, Hashable key) =>
  Socket.Socket ->
  MessagesHandler.KeyGetter key ->
  MessagesHandler.MsgGetter msg ->
  IO (MessagesHandler.MessagesHandler key msg)
runMessagesHandler socket getKey getMsg = do
  let host = Socket.host socket
  let port = Socket.port socket
  let path = Socket.path socket
  stream <- Socket.initStream
  _ <- forkIO $ WS.runClient host port path (Stream.app stream)
  handler <- MessagesHandler.initMessagesHandler stream
  forkIO . forever $ receiver handler getKey getMsg
  pure handler

receiver ::
  (Eq key, Hashable key) =>
  MessagesHandler.MessagesHandler key msg ->
  MessagesHandler.KeyGetter key ->
  MessagesHandler.MsgGetter msg ->
  IO ()
receiver handler getKey getMsg = do
  msg <- Stream.receiveMsg handler
  username <- getKey msg
  oldMsg <- findMsg username handler
  newMsg <- getMsg msg oldMsg
  addMsg username newMsg handler
  pure ()
