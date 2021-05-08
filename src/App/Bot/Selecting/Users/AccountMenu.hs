{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.AccountMenu where

import qualified App.Bot.Execution.Users.Logout as Logout
import qualified App.Bot.Execution.Users.Statistics.Back as Back
import qualified App.Bot.Execution.Users.Statistics.GetStatistics as GetStatistics
import qualified App.Bot.Execution.Users.Statistics.Start as Start
import qualified App.Bot.Execution.Users.Statistics.Stop as Stop
import qualified App.Bot.Execution.Users.Statistics.Subscription as Subscription
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Common.TelegramUserStatus as Common
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import Telegram.Types.Domain.Message (Message)

accountMenu :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
accountMenu callBack msg instId =
  case CallbackQuery.callback_data callBack of
    "Start" -> Start.checkStart msg user instId
    "Stop" -> Stop.execute msg user instId
    "Subscription" -> Subscription.execute msg
    "Statistics" -> GetStatistics.choseStatistics msg user instId
    "Logout" -> Logout.confirmLogout msg user instId
    "Back" -> Back.backToListAccounts msg user
    _ -> do
      Common.setAccountMenu user instId
      Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
