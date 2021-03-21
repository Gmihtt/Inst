{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Logout where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Error (throwLogicError)
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

confirmLogout :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
confirmLogout msg user login = do
  let status = TgUserStatus.TgUser $ TgUserStatus.Logout login
  Common.updateUserStatus user status
  Messages.confirmLogout msg

logout :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
logout msg user instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (ScriptsStat.mkLogoutReq instId)
  liftIO $ Manager.deleteTask instId statManager
  let userId = User.id user
  let tg_id = T.pack $ show userId
  instAcc <-
    Mongo.findInstAccountByInstId tg_id instId "accounts"
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  Mongo.deleteInstAccount tg_id (InstAccount.login instAcc) "accounts"
  Common.putInstAccs userId
  Messages.logout msg
  backListOfAccounts msg user
  where
    errorMsg =
      "Logout.findInstAccountByInstId fail with tg : "
        ++ show user
        ++ " instId: "
        ++ show instId

backListOfAccounts :: Message.Message -> User.User -> Flow (Response Message.Message)
backListOfAccounts msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  Common.updateUserStatus user status
  instAccs <- Common.getInstAccs (User.id user)
  Messages.showInstAccs msg (map InstAccount.login instAccs)

backAccountMenu :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
backAccountMenu msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.updateUserStatus user status
  Messages.accountMenu msg