{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Statistics.GetStatistics where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Error (printDebug)
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.ThreadManager as Manager

choseStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
choseStatistics msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.ChoseStatistics instId
  Common.updateUserStatus user status
  Messages.choseStatistics msg

oneStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
oneStatistics msg user instId = do
  curStatistics <- currentStatistics instId
  Messages.sendStat msg curStatistics
  choseStatistics msg user instId

dayStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
dayStatistics msg user instId = do
  curStatistics <- currentStatistics instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let dayStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByDay time) instStatistics
  Messages.sendStat msg $ curStatistics + dayStat
  choseStatistics msg user instId

weekStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
weekStatistics msg user instId = do
  curStatistics <- currentStatistics instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let weekStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByWeek time) instStatistics
  Messages.sendStat msg $ curStatistics + weekStat
  choseStatistics msg user instId

monthStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
monthStatistics msg user instId = do
  curStatistics <- currentStatistics instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let monthStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByMonth time) instStatistics
  Messages.sendStat msg $ curStatistics + monthStat
  choseStatistics msg user instId

currentStatistics :: Text -> Flow Int
currentStatistics instId = do
  env <- getEnvironment
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  let value = maybe 0 Statistic.getSize mbStat
  liftIO $ printDebug ("Current statistics = " <> T.pack (show value) <> " For instId: " <> instId)
  pure value