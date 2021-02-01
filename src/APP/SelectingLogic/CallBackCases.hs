{-# LANGUAGE OverloadedStrings #-}

module APP.SelectingLogic.CallBackCases where

import qualified APP.Telegram.Login.Login as Login
import qualified APP.Telegram.Messages.FlowMessages as Messages
import APP.Telegram.SendMessage (sendMessage)
import Common.Flow (Flow)
import qualified Common.Transforms as Common
import qualified Types.Domain.Status.LoginStatus as LoginStatus
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.CallbackQuery as CallbackQuery
import Types.Telegram.Types.Message (Message)
import qualified Types.Telegram.Types.Message as Message
import qualified Types.Telegram.Types.User as User

execute :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
execute callBack msg =
  case CallbackQuery.callback_data callBack of
    "login" -> Login.execute msg LoginStatus.Free userId
    "run" -> Messages.baseMenu msg
    "stop" -> Messages.baseMenu msg
    "payment" -> Messages.baseMenu msg
    "statistics" -> Messages.baseMenu msg
  where
    userId = User.id $ CallbackQuery.callback_from callBack
