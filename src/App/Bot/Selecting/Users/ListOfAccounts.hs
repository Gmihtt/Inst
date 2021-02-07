{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.ListOfAccounts where

import qualified App.Bot.Execution.Users.ShowAccounts as ShowAccounts
import qualified App.Bot.Messages.FlowMessages as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Common.Flow (Flow)
import qualified Data.Text as T
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified MongoDB.Queries as Mongo

listOfAccounts :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
listOfAccounts callBack msg =
  case CallbackQuery.callback_data callBack of
    "Add" -> ShowAccounts.addAccount msg userId 
    "Back" -> ShowAccounts.back msg userId 
    username -> do
      let uId = T.pack $ show userId
      mbInstAcc <- Mongo.findInstAccountByLogin uId username "accounts"
      maybe (Messages.strangeMessage msg) (ShowAccounts.selectedAcc msg userId) mbInstAcc
  where
    userId = User.id $ CallbackQuery.callback_from callBack