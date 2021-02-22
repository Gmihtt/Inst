{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Statistics where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import qualified Control.Concurrent.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified MongoDB.Queries as QMongo
import qualified Redis.Queries as Redis
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Communication.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Telegram.Types.Domain.User as User

start :: Message.Message -> Text -> Flow (Response Message.Message)
start msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStartReq instId)
  Messages.start msg

stop :: Message.Message -> Text -> Flow (Response Message.Message)
stop msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStopReq instId)
  Messages.stop msg

stat :: Message.Message -> Text -> Flow (Response Message.Message)
stat msg instId = do
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
