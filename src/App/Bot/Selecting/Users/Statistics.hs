{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.Statistics where

import qualified App.Bot.Execution.Users.Statistics.Back as Back
import qualified App.Bot.Execution.Users.Statistics.GetStatistics as GetStatistics
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Types.Domain.InstAccount as InstAccount

statistics :: CallbackQuery.CallbackQuery -> Message -> InstAccount.InstId -> Flow (Response Message)
statistics callBack msg instId =
  case CallbackQuery.callback_data callBack of
    "Current" -> GetStatistics.oneStatistics msg user instId
    "Day" -> GetStatistics.dayStatistics msg user instId
    "7 Days" -> GetStatistics.weekStatistics msg user instId
    "30 Days" -> GetStatistics.monthStatistics msg user instId
    "Back" -> Back.backToAccountMenu msg user instId
    _ -> do
      Common.setChoseStatistics user instId
      Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
