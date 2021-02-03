{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( InlineKeyboardButton (..),
    mkInlineKeyboardButton,
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    Value,
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data InlineKeyboardButton
  = InlineKeyboardButton
      { text :: Text,
        url :: Maybe Text,
        callback_data :: Maybe Text,
        switch_inline_query :: Maybe Text,
        switch_inline_query_current_chat :: Maybe Text,
        callback_game :: Maybe Value
      }
  deriving (Show, Eq, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJson

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJson

mkInlineKeyboardButton :: Text -> Maybe Text -> Maybe Text -> InlineKeyboardButton
mkInlineKeyboardButton keyboard_text keyboard_url callback =
  InlineKeyboardButton
    { text = keyboard_text,
      url = keyboard_url,
      callback_data = callback,
      switch_inline_query = Nothing,
      switch_inline_query_current_chat = Nothing,
      callback_game = Nothing
    }
