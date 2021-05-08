{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.Confirm where

import qualified App.Bot.Execution.Users.Logout as Logout
import qualified App.Bot.Execution.Users.Statistics.Start as Start
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)

logout :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
logout callBack msg instId = do
  liftIO $ print $ CallbackQuery.callback_data callBack
  case CallbackQuery.callback_data callBack of
    "Yes" -> Logout.logout msg user instId
    "No" -> Logout.backAccountMenu msg user instId
    _ -> do
      Common.setLogout user instId
      Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack

start :: CallbackQuery.CallbackQuery -> Message -> Text -> Flow (Response Message)
start callBack msg instId = do
  liftIO $ print $ CallbackQuery.callback_data callBack
  case CallbackQuery.callback_data callBack of
    "Yes" -> Start.start True msg user instId
    "No" -> Start.start False msg user instId
    _ -> do
      Common.setWaitStart user instId
      Messages.strangeMessage msg
  where
    user = CallbackQuery.callback_from callBack
