{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Keyboard.InlineKeyboardMarkup (InlineKeyboardMarkup, mkInlineKeyboardMarkup) where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    Value,
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Telegram.Types.Keyboard.InlineKeyboardButton (InlineKeyboardButton(..), mkInlineKeyboardButton)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {
  inline_keyboard :: [[InlineKeyboardButton]]
}deriving (Show, Eq, Generic)

instance ToJSON InlineKeyboardMarkup where
  toJSON = toJson

instance FromJSON InlineKeyboardMarkup where
  parseJSON = parseJson

mkInlineKeyboardMarkup :: [[InlineKeyboardButton]] -> InlineKeyboardMarkup
mkInlineKeyboardMarkup = InlineKeyboardMarkup