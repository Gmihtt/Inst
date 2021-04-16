{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Statistics.API where

import qualified App.Scripts.Socket.Connection as Connection
import Common.Error (printDebug, printError, throwSocketErr)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import qualified Types.Communication.Statistics.Request as RequestStat
import qualified Types.Communication.Statistics.Response as ResponseStat
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Statistic as Statistic

statConnection :: Socket.Socket -> IO Manager.StatisticsManager
statConnection socket = do
  liftIO $ Connection.runConnection socket getUsername mkStatistics
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseStat.response_user_id) (decode bsBody)

mkStatistics :: ByteString -> Maybe Statistic.Statistic -> IO Statistic.Statistic
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  users <- getUsers value
  pure $ maybe (addUsers users Statistic.empty) (addUsers users) mbStat
  where
    addUsers users stat = foldr Statistic.addUser stat users
    getUsers value =
      if not $ ResponseStat.response_status value
        then printError (("Error : " <>) . unpack <$> ResponseStat.response_errorMessage value) >> pure []
        else pure . fromMaybe [] $ ResponseStat.response_users value

sendMsg :: Manager.StatisticsManager -> RequestStat.Request -> IO ()
sendMsg manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
