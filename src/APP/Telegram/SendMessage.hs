{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.SendMessage where

import qualified API.Routes as API
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Common.Error (throwTelegramErr)
import Network.HTTP.Client (Manager)
import Types.Telegram.Response (Response(..))
import qualified Types.Telegram.Types.Message as Message
import Types.Telegram.Methods.SendMessage ( mkSendMessage , ReplyMarkup(..) )
import Types.Telegram.Types.Keyboard.InlineKeyboardButton
    ( InlineKeyboardButton, mkInlineKeyboardButton )
import Types.Telegram.Types.Keyboard.InlineKeyboardMarkup
    ( mkInlineKeyboardMarkup )

sendMessage :: Manager -> Text -> Message.Message -> IO (Response Message.Message)
sendMessage manager token message = do
  let markup = Just $ mkInlineKeyboardMarkup [[login, run, stop, payment, statistics]]
  let sM = mkSendMessage message markup
  API.callTelegram (API.sendMessage token sM) manager

login :: InlineKeyboardButton
login = mkInlineKeyboardButton "login" (Just "https://vk.com/thekone4no") Nothing

run :: InlineKeyboardButton
run = mkInlineKeyboardButton "run" (Just "https://vk.com/thekone4no") Nothing

stop :: InlineKeyboardButton
stop = mkInlineKeyboardButton "stop" (Just "https://vk.com/thekone4no") Nothing

payment :: InlineKeyboardButton
payment = mkInlineKeyboardButton "payment" (Just "https://vk.com/thekone4no") Nothing

statistics :: InlineKeyboardButton
statistics = mkInlineKeyboardButton "statistics" (Just "https://vk.com/thekone4no") Nothing