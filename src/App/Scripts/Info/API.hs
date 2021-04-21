{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Info.API where

import Common.Error (printDebug, printError, throwSocketErr)
import qualified Communication.Sockets.API as SocketAPI
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import qualified Types.Communication.Info.Request as RequestInfo
import qualified Types.Communication.Info.Response as ResponseInfo
import qualified Types.Domain.ThreadManager as Manager

infoConnection :: SocketAPI.Socket -> IO Manager.InfoManager
infoConnection socket = do
  liftIO $ SocketAPI.runConnection socket getUsername getBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseInfo.admin_id) (decode bsBody)
    getBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendMsg :: Manager.StatisticsManager -> RequestInfo.Request -> IO ()
sendMsg manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
