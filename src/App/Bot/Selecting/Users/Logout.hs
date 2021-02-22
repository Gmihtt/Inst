{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.Logout where

import qualified App.Bot.Execution.Users.Logout as Logout
import qualified App.Bot.Execution.Users.Statistics as Statistics
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified MongoDB.Queries as Mongo
import Telegram.API.Methods.SendMessage (sendMessage)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

logout :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
logout callBack msg instId = do
  liftIO $ print $ CallbackQuery.callback_data callBack
  case CallbackQuery.callback_data callBack of
    "Yes" -> Logout.logout msg userId instId
    "No" -> Logout.backAccountMenu msg userId instId
    _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from callBack
