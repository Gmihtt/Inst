{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Admin.SelectingTgUser where

import qualified App.Bot.Execution.Users.Statistics.Start as Start
import qualified App.Bot.Execution.Users.Statistics.Stop as Stop
import qualified App.Bot.Execution.Users.Statistics.Subscription as Subscription
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Types.Domain.InstAccount as InstAccount

selectMenu :: CallbackQuery.CallbackQuery -> Message -> InstAccount.InstId -> Flow (Response Message)
selectMenu callBack msg instId =
  case CallbackQuery.callback_data callBack of
    "Найти по telegram" -> Start.checkStart msg user instId
    "Найти по instagram" -> Stop.execute msg user instId
    "Количество активных пользователей" -> Stop.execute msg user instId
    "Назад" -> Subscription.execute msg
    _ -> Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
