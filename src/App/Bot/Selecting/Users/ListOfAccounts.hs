{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.ListOfAccounts where

import qualified App.Bot.Execution.Users.ShowAccounts as ShowAccounts
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser

listOfAccounts :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
listOfAccounts callBack msg =
  case CallbackQuery.callback_data callBack of
    "Add" -> ShowAccounts.addAccount msg user
    "Back" -> ShowAccounts.back msg user
    username -> do
      let uId = TgUser.TgId . T.pack $ show userId
      mbInstAcc <- Mongo.findInstAccountByLogin uId (InstAccount.InstUsername username)
      maybe (Common.setListOfAccounts user >> Messages.strangeMessage msg) (ShowAccounts.selectedAcc msg user) mbInstAcc
  where
    user = CallbackQuery.callback_from callBack
    userId = User.id user
