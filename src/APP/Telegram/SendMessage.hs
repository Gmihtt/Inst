{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.SendMessage where

import qualified API.Routes as API
import qualified APP.Telegram.Buttons as Buttons
import Common.Error (throwTelegramErr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Types.Telegram.Methods.SendMessage (ReplyMarkup (..), mkSendMessage)
import Types.Telegram.Response (Response (..))
import Types.Telegram.Types.Keyboard.InlineKeyboardButton
  ( mkInlineKeyboardButton,
  )
import Types.Telegram.Types.Keyboard.InlineKeyboardMarkup
  ( mkInlineKeyboardMarkup,
  )
import qualified Types.Telegram.Types.Message as Message

sendMessage :: Manager -> Text -> Message.Message -> IO (Response Message.Message)
sendMessage manager token message = do
  let markup = Just $ mkInlineKeyboardMarkup [[Buttons.login, Buttons.run, Buttons.stop, Buttons.payment, Buttons.statistics]]
  let sM = mkSendMessage message markup
  API.callTelegram (API.sendMessage token sM) manager
