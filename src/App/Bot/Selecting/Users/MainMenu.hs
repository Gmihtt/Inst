{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.MainMenu where

import qualified App.Bot.Execution.Users.MainMenu as MainMenu
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import qualified Telegram.Types.Domain.Message as Message

mainMenu :: CallbackQuery.CallbackQuery -> Message.Message -> Flow (Response Message.Message)
mainMenu callBack msg =
  case CallbackQuery.callback_data callBack of
    "Accounts" -> MainMenu.accounts msg user
    "Help" -> MainMenu.help msg user
    _ -> Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
