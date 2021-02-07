{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Statistics.API where

import qualified App.Scripts.Socket.API as API
import qualified App.Scripts.Socket.Connection as Connection
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr, printError, printDebug)
import Common.Flow (Flow)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Types.Domain.Manager as Manager
import qualified Types.Communication.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Statistic as Statistic

statConnection :: Socket.Socket -> IO Manager.StatisticsManager
statConnection socket = do
  liftIO $ Connection.runConnection socket getUsername mkStatistics
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ScriptsStat.response_inst_id) (decode bsBody)

mkStatistics :: ByteString -> Maybe Statistic.Statistic -> IO Statistic.Statistic
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  users <- getUsers value
  pure $ maybe (addUsers users Statistic.empty) (addUsers users) mbStat
  where
    addUsers users stat = foldr Statistic.addUser stat users
    getUsers value =
      if not $ ScriptsStat.response_status value
        then printError (("Error : " <>) . unpack <$> ScriptsStat.response_errorMessage value) >> pure []
        else pure . fromMaybe [] $ ScriptsStat.response_users value

sendMsg :: Manager.StatisticsManager -> ScriptsStat.Request -> IO ()
sendMsg manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
