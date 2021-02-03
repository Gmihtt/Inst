{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import GHC.Generics (Generic)
import Telegram.Types.Domain.Keyboard.InlineKeyboardButton (InlineKeyboardButton (..))

newtype InlineKeyboardMarkup
  = InlineKeyboardMarkup
      { inline_keyboard :: [[InlineKeyboardButton]]
      }
  deriving (Show, Eq, Generic)

instance ToJSON InlineKeyboardMarkup where
  toJSON = toJson

instance FromJSON InlineKeyboardMarkup where
  parseJSON = parseJson

mkInlineKeyboardMarkup :: [[InlineKeyboardButton]] -> InlineKeyboardMarkup
mkInlineKeyboardMarkup = InlineKeyboardMarkup
