module App.Bot.Execution.Users.Statistics.Start where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Statistics.Request as RequestStat
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.ThreadManager as Manager

checkStart :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
checkStart msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.WaitStart instId
  Common.updateUserStatus user status
  Messages.continueStat msg

start :: Bool -> Message.Message -> User.User -> Text -> Flow (Response Message.Message)
start False msg user instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (RequestStat.mkStartReq instId)
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.updateUserStatus user status
  Messages.start msg
start True msg user instId = do
  env <- getEnvironment
  mbInstStat <- Mongo.findInstStatById instId
  case mbInstStat of
    Just instStat -> do
      let statManager = Environment.statisticsManager env
      liftIO $ API.sendMsg statManager (RequestStat.mkStartReq instId)
      let statistics = Statistic.initWithLastUsers $ InstStatistics.lastCountUsers instStat
      liftIO $ Manager.addTask instId statistics statManager
      let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
      Common.updateUserStatus user status
      Messages.start msg
    Nothing -> do
      Messages.lastCountUsersNotFound msg
      start False msg user instId
