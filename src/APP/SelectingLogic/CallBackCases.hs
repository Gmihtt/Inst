{-# LANGUAGE OverloadedStrings #-}

module APP.SelectingLogic.CallBackCases where

import qualified Common.Transforms as Common
import Common.Flow (Flow)
import Types.Telegram.Response (Response (..))
import qualified APP.Telegram.Login.Login as Login
import Types.Telegram.Types.Message (Message)
import qualified Types.Telegram.Types.CallbackQuery as CallbackQuery
import qualified Types.Domain.Status.LoginStatus as LoginStatus
import qualified Types.Telegram.Types.User as User
import qualified Types.Telegram.Types.Message as Message
import qualified APP.Telegram.Messages.FlowMessages as Messages
import APP.Telegram.SendMessage ( sendMessage )

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