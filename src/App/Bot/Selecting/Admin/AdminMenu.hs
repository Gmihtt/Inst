{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Admin.AdminMenu where

import qualified App.Bot.Execution.Admin.AdminMenu as Admin
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)

adminMenu :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
adminMenu callBack msg =
  case CallbackQuery.callback_data callBack of
    "Управление пользователями" -> Admin.selectUser msg user
    "Управление администраторами" -> Admin.selectAdmin msg user
    "Вернуться в меню пользователя" -> Admin.back msg user
    _ -> Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
