module App.Bot.Execution.Users.Statistics.GetStatistics where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

choseStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
choseStatistics msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.ChoseStatistics instId
  Common.updateUserStatus user status
  Messages.choseStatistics msg

oneStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
oneStatistics msg user instId = do
  curStatistics <- currentStatistics user instId
  Messages.sendStat msg curStatistics

dayStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
dayStatistics msg user instId = do
  curStatistics <- currentStatistics user instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let dayStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByDay time) instStatistics
  Messages.sendStat msg $ curStatistics + dayStat

weekStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
weekStatistics msg user instId = do
  curStatistics <- currentStatistics user instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let weekStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByWeek time) instStatistics
  Messages.sendStat msg $ curStatistics + weekStat

monthStatistics :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
monthStatistics msg user instId = do
  curStatistics <- currentStatistics user instId
  time <- liftIO getCurrentTime
  instStatistics <- Mongo.findInstStatById instId
  let monthStat = fromIntegral $ maybe 0 (InstStatistics.groupCountByMonth time) instStatistics
  Messages.sendStat msg $ curStatistics + monthStat

currentStatistics :: User.User -> Text -> Flow Int
currentStatistics user instId = do
  env <- ask
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.updateUserStatus user status
  pure $ maybe 0 Statistic.getSize mbStat
