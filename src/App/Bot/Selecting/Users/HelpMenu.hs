{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.HelpMenu where

import qualified App.Bot.Execution.Users.Help as Help
import qualified App.Bot.Messages.FlowMessages as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Common.Flow (Flow)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

helpMenu :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
helpMenu callBack msg =
  case CallbackQuery.callback_data callBack of
    "Back" -> Help.back msg userId
    _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from callBack