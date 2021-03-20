{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Statistics where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

checkStart :: Message.Message -> Text -> Flow (Response Message.Message)
checkStart msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStartReq instId)
  Messages.continueStat msg

start :: Bool -> Message.Message -> Text -> Flow (Response Message.Message)
start False msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStartReq instId)
  Messages.start msg
start True msg instId = do
  env <- ask
  mbInstStat <- Mongo.findInstStatById instId
  case mbInstStat of
    Just instStat -> do
      let statManager = Environment.statisticsManager env
      liftIO $ API.sendMsg statManager (ScriptsStat.mkStartReq instId)
      let statistics = Statistic.initWithLastUsers $ InstStatistics.lastCountUsers instStat
      liftIO $ Manager.addTask instId statistics statManager
      Messages.start msg
    Nothing -> do
      Messages.lastCountUsersNotFound msg
      start False msg instId

stop :: Message.Message -> Text -> Flow (Response Message.Message)
stop msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStopReq instId)
  saveStat instId
  Messages.stop msg

getStatistic :: Message.Message -> Text -> Flow (Response Message.Message)
getStatistic msg instId = do
  env <- ask
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  Messages.sendStat msg $ maybe 0 Statistic.getSize mbStat

subscription :: Message.Message -> Flow (Response Message.Message)
subscription = Messages.todoMsg

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  Common.updateUserStatus user status
  instAccs <- Common.getInstAccs (User.id user)
  Messages.showInstAccs msg (map InstAccount.login instAccs)

saveStat :: Text -> Flow ()
saveStat instId = do
  env <- ask
  let manager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId manager
  case mbStat of
    Just stat -> do
      finish <- liftIO getCurrentTime
      let count = Statistic.getSize stat
      let statistic = InstStatistics.mkStatistic (fromIntegral count) finish
      let lastCountUsers = Statistic.getLastUsers stat
      let newInstStat = InstStatistics.mkInstStatistics instId [statistic] lastCountUsers
      oldInstStat <- Mongo.findInstStatById instId
      let instStat = maybe newInstStat (updateInstStat statistic lastCountUsers) oldInstStat
      Mongo.updateInstStat instId instStat
    Nothing -> pure ()
  where
    updateInstStat statistic lastCountUsers oldInstStat =
      InstStatistics.addStatistic
        oldInstStat {InstStatistics.lastCountUsers = lastCountUsers}
        statistic
