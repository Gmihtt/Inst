{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Telegram.Methods.SendMessage
  ( SendMessage,
    mkSendMessage,
    ReplyMarkup(..)
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    Value,
    parseJson,
    toJson,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Telegram.Types.Chat as Chat
import Types.Telegram.Types.Keyboard.ForceReply (ForceReply)
import Types.Telegram.Types.Keyboard.ReplyKeyboardHide (ReplyKeyboardHide)
import Types.Telegram.Types.Keyboard.ReplyKeyboardMarkup (ReplyKeyboardMarkup)
import Types.Telegram.Types.Keyboard.InlineKeyboardMarkup (InlineKeyboardMarkup)
import qualified Types.Telegram.Types.Message as Message

data ReplyMarkup
  = InlineKeyboard InlineKeyboardMarkup
  | Keyboard ReplyKeyboardMarkup
  | Hide ReplyKeyboardHide
  | Force ForceReply
  deriving (Show, Eq, Generic)

instance ToJSON ReplyMarkup

{-toJSON (InlineKeyboard v) = toJSON v
toJSON (Keyboard v) = toJSON v
toJSON (Hide v) = toJSON v
toJSON (Force v) = toJSON v-}

instance FromJSON ReplyMarkup

{-parseJSON ReplyKeyboardMarkup = toJSON v
toJSON (Hide v) = toJSON v
toJSON (Force v) = toJSON v-}

data ParseMode = Markdown | MarkdownV2 | HTML deriving (Show, Eq, Generic)

instance ToJSON ParseMode

instance FromJSON ParseMode

data SendMessage
  = SendMessage
      { chat_id :: Int,
        text :: Text,
        parse_mode :: Maybe ParseMode,
        disable_web_page_preview :: Maybe Bool,
        disable_notification :: Maybe Bool,
        reply_to_message_id :: Maybe Int,
        reply_markup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic)

instance ToJSON SendMessage where
  toJSON = toJson

instance FromJSON SendMessage where
  parseJSON = parseJson

mkSendMessage :: Message.Message -> Maybe InlineKeyboardMarkup -> SendMessage
mkSendMessage message mbMarkup =
  SendMessage
    { chat_id = Chat.chat_id $ Message.chat message,
      text = fromMaybe "" $ Message.text message,
      parse_mode = Nothing,
      disable_web_page_preview = Nothing,
      disable_notification = Nothing,
      reply_to_message_id = Nothing,
      reply_markup = mbMarkup
    }
