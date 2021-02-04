{-# LANGUAGE OverloadedStrings #-}

module App.SelectingLogic.CallBackCases where

import qualified App.Bot.Login.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Common.Flow (Flow)
import qualified Common.Transforms as Common
import qualified Types.Domain.Status.LoginStatus as LoginStatus
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import qualified App.Bot.Statistics.Statistics as Statistics
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

execute :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
execute callBack msg =
  case CallbackQuery.callback_data callBack of
    "login" -> Login.execute msg LoginStatus.Free userId
    "run" -> Statistics.start msg userId
    "stop" -> Statistics.stop msg userId
    "payment" -> Messages.baseMenu msg
    "statistics" -> Statistics.stat msg userId
  where
    userId = User.id $ CallbackQuery.callback_from callBack
