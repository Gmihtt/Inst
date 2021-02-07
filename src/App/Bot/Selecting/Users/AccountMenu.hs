{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.AccountMenu where

import qualified App.Bot.Execution.Users.Statistics as Statistics
import qualified App.Bot.Messages.FlowMessages as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Control.Monad.IO.Class (liftIO)
import Common.Flow (Flow)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Common.FlowEnv as Common
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified MongoDB.Queries as Mongo

accountMenu :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
accountMenu callBack msg instAcc =
  case CallbackQuery.callback_data callBack of
    "Start" -> Statistics.start msg userId instAcc
    "Stop" -> Statistics.stop msg userId instAcc
    "Subscription" -> Statistics.subscription msg
    "Statistics" -> Statistics.stat msg userId instAcc
    "Logout" -> Statistics.logout msg
    "Back" -> Statistics.back msg userId
    _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from callBack