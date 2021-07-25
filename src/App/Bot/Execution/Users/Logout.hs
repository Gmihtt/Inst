{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Logout where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Error (throwLogicError)
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import qualified Communication.Scripts.Statistics.API as API
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser

confirmLogout :: Message.Message -> User.User -> InstAccount.InstId -> Flow (Response Message.Message)
confirmLogout msg user instId = do
  Common.setLogout user instId
  Messages.confirmLogout msg

logout :: Message.Message -> User.User -> InstAccount.InstId -> Flow (Response Message.Message)
logout msg user instId = do
  env <- getEnvironment
  let handler = Environment.statisticsMessagesHandler env
  liftIO $ API.sendMsg handler (RequestStat.mkLogoutReq (InstAccount.id instId))
  Save.execute msg instId
  let userId = User.id user
  let tgId = TgUser.TgId . T.pack $ show userId
  instAcc <-
    Mongo.findInstAccountByInstId tgId instId
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  Mongo.deleteInstAccount tgId (InstAccount.instUsername instAcc)
  Common.putInstAccsByTgUserId userId
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
  instAccs <- Common.getInstAccsByTgUserId (User.id user)
  Messages.showInstAccs msg (map InstAccount.instUsername instAccs)

backAccountMenu :: Message.Message -> User.User -> InstAccount.InstId -> Flow (Response Message.Message)
backAccountMenu msg user instId = do
  Common.setAccountMenu user instId
  Messages.accountMenu msg
