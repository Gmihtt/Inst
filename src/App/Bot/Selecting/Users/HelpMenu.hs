{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.HelpMenu where

import qualified App.Bot.Execution.Users.Help as Help
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)

helpMenu :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
helpMenu callBack msg =
  case CallbackQuery.callback_data callBack of
    "Back" -> Help.back msg user
    _ -> do
      Common.setHelp user
      Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
