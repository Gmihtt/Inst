{-# LANGUAGE OverloadedStrings #-}

module Communication.Scripts.Info.API where

import Common.Error (printDebug, throwSocketErr)
import qualified Communication.Scripts.MessageHandler.API as MessageHandlerAPI
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Types.Communication.Scripts.Info.Request as RequestInfo
import qualified Types.Communication.Scripts.Info.Response as ResponseInfo
import qualified Types.Domain.ThreadManager as Manager

infoConnection :: SocketAPI.Socket -> IO Manager.InfoManager
infoConnection socket = do
  liftIO $ SocketAPI.runConnection socket getUsername getBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseInfo.admin_id) (decode bsBody)
    getBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> Manager.InfoManager -> RequestInfo.Request -> IO ResponseInfo.Response
sendAndReceiveMsg key manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
  script <- newEmptyMVar
  getMsg script
  res <- takeMVar script
  Manager.deleteTask key manager
  printDebug res
  pure res
  where
    getMsg script = do
      sleepSecond
      mbMsg <- Manager.findTask key manager
      maybe (getMsg script) (putMVar script) mbMsg
    sleepSecond = threadDelay 1000000
