{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Logout where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Error (throwLogicError)
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.InstAccount as InstAccount

confirmLogout :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
confirmLogout msg user accLogin = do
  Common.setLogout user accLogin
  Messages.confirmLogout msg

logout :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
logout msg user instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (RequestStat.mkLogoutReq instId)
  Save.execute msg instId
  let userId = User.id user
  let tg_id = T.pack $ show userId
  instAcc <-
    Mongo.findInstAccountByInstId tg_id instId
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  Mongo.deleteInstAccount tg_id (InstAccount.login instAcc)
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
  Common.setListOfAccounts user
  instAccs <- Common.getInstAccs (User.id user)
  Messages.showInstAccs msg (map InstAccount.login instAccs)

backAccountMenu :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
backAccountMenu msg user instId = do
  Common.setAccountMenu user instId
  Messages.accountMenu msg
