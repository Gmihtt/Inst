{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Statistics.API where

import Common.Error (printDebug, printError, throwSocketErr)
import qualified Communication.Sockets.API as SocketAPI
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack)
import qualified Types.Communication.Statistics.Request as RequestStat
import qualified Types.Communication.Statistics.Response as ResponseStat
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.ThreadManager as Manager

statConnection :: SocketAPI.Socket -> IO Manager.StatisticsManager
statConnection socket = do
  liftIO $ SocketAPI.runConnection socket getUsername mkStatistics
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseStat.inst_id) (decode bsBody)

mkStatistics :: ByteString -> Maybe Statistic.Statistic -> IO Statistic.Statistic
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  users <- getUsers value
  pure $ maybe (addUsers users Statistic.empty) (addUsers users) mbStat
  where
    addUsers users stat = foldr Statistic.addUser stat users
    getUsers value =
      maybe
        (printError (("Error : " <>) . unpack <$> ResponseStat.error_message value) >> pure [])
        pure
        (ResponseStat.users value)

sendMsg :: Manager.StatisticsManager -> RequestStat.Request -> IO ()
sendMsg manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
