module Communication.Scripts.MessageHandler.API (module Stream, module Messages) where

import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.MessagesHandler as MessagesHandler
import Communication.Scripts.MessageHandler.Stream as Stream
    ( sendMsg, receiveMsg, app )
import Communication.Scripts.MessageHandler.Messages as Messages
    ( addMsg, findMsg, deleteMsg )
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Network.WebSockets as WS

runMessagesHandler ::
  Socket.Socket ->
  MessagesHandler.UsernameGetter ->
  MessagesHandler.MsgGetter msg ->
  IO (MessagesHandler.MessagesHandler msg)
runMessagesHandler socket getUsername getMsg = do
  let host = Socket.host socket
  let port = Socket.port socket
  let path = Socket.path socket
  stream <- Socket.initStream
  _ <- forkIO $ WS.runClient host port path (Stream.app stream)
  handler <- MessagesHandler.initMessagesHandler stream
  forkIO . forever $ receiver handler getUsername getMsg
  pure handler

receiver ::
  MessagesHandler.MessagesHandler msg ->
  MessagesHandler.UsernameGetter ->
  MessagesHandler.MsgGetter msg ->
  IO ()
receiver handler getUsername getMsg = do
  msg <- Stream.receiveMsg handler
  username <- getUsername msg
  oldMsg <- findMsg username handler
  newMsg <- getMsg msg oldMsg
  addMsg username newMsg handler
  pure ()