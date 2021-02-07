{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.MainMenu where

import qualified App.Bot.Execution.Users.MainMenu as MainMenu
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import Telegram.API.Methods.SendMessage (sendMessage)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

mainMenu :: CallbackQuery.CallbackQuery -> Message.Message -> Flow (Response Message.Message)
mainMenu callBack msg =
  case CallbackQuery.callback_data callBack of
    "Accounts" -> MainMenu.accounts msg userId
    "Help" -> MainMenu.help msg userId
    _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from callBack
