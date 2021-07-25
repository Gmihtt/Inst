{-# LANGUAGE OverloadedStrings #-}

module Communication.Scripts.Info.API where

import Common.Error (printDebug, throwSocketErr)
import qualified Communication.Scripts.MessagesHandler.API as MessagesHandlerAPI
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Types.Communication.Scripts.Info.Request as RequestInfo
import qualified Types.Communication.Scripts.Info.Response as ResponseInfo
import qualified Types.Domain.Socket as Socket

type InfoMessagesHandler = MessagesHandlerAPI.MessagesHandler Text ResponseInfo.Response

infoConnection :: Socket.Socket -> IO InfoMessagesHandler
infoConnection socket = do
  liftIO $ MessagesHandlerAPI.runMessagesHandler socket getAdminId getBody
  where
    getAdminId bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseInfo.admin_id) (decode bsBody)
    getBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> InfoMessagesHandler -> RequestInfo.Request -> IO ResponseInfo.Response
sendAndReceiveMsg adminId handler req = do
  printDebug req
  MessagesHandlerAPI.sendMsg (encode req) handler
  v <- newEmptyMVar
  getMsg v
  res <- takeMVar v
  MessagesHandlerAPI.deleteMsg adminId handler
  printDebug res
  pure res
  where
    getMsg v = do
      sleepSecond
      mbMsg <- MessagesHandlerAPI.findMsg adminId handler
      maybe (getMsg v) (putMVar v) mbMsg
    sleepSecond = threadDelay 1000000
