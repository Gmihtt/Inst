{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Logout where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Error (throwLogicError)
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

confirmLogout :: Message.Message -> Int -> Text -> Flow (Response Message.Message)
confirmLogout msg userId login = do
  let status = TgUserStatus.TgUser $ TgUserStatus.Logout login
  Common.updateUserStatus userId status
  Messages.confirmLogout msg

logout :: Message.Message -> Int -> Text -> Flow (Response Message.Message)
logout msg userId instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkLogoutReq instId)
  let tg_id = T.pack $ show userId
  instAcc <-
    Mongo.findInstAccountByInstId tg_id instId "accounts"
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  Mongo.deleteInstAccount tg_id (InstAccount.login instAcc) "accounts"
  Common.putInstAccs userId
  Messages.logout msg
  backListOfAccounts msg userId
  where
    errorMsg =
      "Logout.findInstAccountByInstId fail with tg_id : "
        ++ show userId
        ++ " instId: "
        ++ show instId

backListOfAccounts :: Message.Message -> Int -> Flow (Response Message.Message)
backListOfAccounts msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  Common.updateUserStatus userId status
  instAccs <- Common.getInstAccs userId
  Messages.showInstAccs msg (map InstAccount.login instAccs)

backAccountMenu :: Message.Message -> Int -> Text -> Flow (Response Message.Message)
backAccountMenu msg userId instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.updateUserStatus userId status
  Messages.accountMenu msg
