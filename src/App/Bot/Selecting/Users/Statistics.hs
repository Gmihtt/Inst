{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.Statistics where

import qualified App.Bot.Execution.Users.Statistics.GetStatistics as GetStatistics
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)

statistics :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
statistics callBack msg instAcc =
  case CallbackQuery.callback_data callBack of
    "Current" -> GetStatistics.oneStatistics msg user instAcc
    "Day" -> GetStatistics.dayStatistics msg user instAcc
    "7 Days" -> GetStatistics.weekStatistics msg user instAcc
    "30 Days" -> GetStatistics.monthStatistics msg user instAcc
    _ -> Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
