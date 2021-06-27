{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Statistics.GetStatistics where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Error as Error
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.ThreadManager as Manager

choseStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
choseStatistics msg user instId = do
  Common.setChoseStatistics user instId
  Messages.choseStatistics msg

oneStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
oneStatistics msg user instId = do
  eCurStatistics <- currentStatistics instId
  case eCurStatistics of
    Right curStatistics -> do
      Common.setChoseStatistics user instId
      Messages.sendStat msg curStatistics
    Left err -> Messages.smthMessage err msg

dayStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
dayStatistics msg user instId = groupStatistics msg user instId InstStatistics.groupCountByDay

weekStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
weekStatistics msg user instId = groupStatistics msg user instId InstStatistics.groupCountByWeek

monthStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
monthStatistics msg user instId = groupStatistics msg user instId InstStatistics.groupCountByMonth

groupStatistics ::
  Message.Message ->
  User.User ->
  Text ->
  (UTCTime -> InstStatistics.InstStatistics -> Int32) ->
  Flow (Response Message.Message)
groupStatistics msg user instId group = do
  eCurStatistics <- currentStatistics instId
  case eCurStatistics of
    Right curStatistics -> do
      time <- liftIO getCurrentTime
      instStatistics <- Mongo.findInstStatById instId
      let monthStat = fromIntegral $ maybe 0 (group time) instStatistics
      Common.setChoseStatistics user instId
      Messages.sendStat msg $ curStatistics + monthStat
    Left err -> Messages.smthMessage err msg

currentStatistics :: Text -> Flow (Either Error.Error Int)
currentStatistics instId = do
  env <- getEnvironment
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  let value = maybe (Right 0) (Statistic.getSize <$>) mbStat
  pure value
