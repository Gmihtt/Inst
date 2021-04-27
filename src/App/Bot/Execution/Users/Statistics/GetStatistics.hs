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
import Data.Int ( Int32 )
import Data.Time (getCurrentTime, UTCTime)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.ThreadManager as Manager
import qualified Types.Communication.Error as Error

choseStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
choseStatistics msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.ChoseStatistics instId
  Common.updateUserStatus user status
  Messages.choseStatistics msg

oneStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
oneStatistics msg user instId = do
  eCurStatistics <- currentStatistics instId
  case eCurStatistics of
    Right curStatistics -> do 
      Messages.sendStat msg curStatistics
      choseStatistics msg user instId
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
      Messages.sendStat msg $ curStatistics + monthStat
      choseStatistics msg user instId
    Left err -> Messages.smthMessage err msg

currentStatistics :: Text -> Flow (Either Error.Error Int)
currentStatistics instId = do
  env <- getEnvironment
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  let value = maybe (Right 0) (Statistic.getSize <$>) mbStat
  pure value