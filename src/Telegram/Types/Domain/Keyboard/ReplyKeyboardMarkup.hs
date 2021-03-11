{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.ReplyKeyboardMarkup where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import GHC.Generics (Generic)
import Telegram.Types.Domain.Keyboard.KeyboardButton (KeyboardButtons)

data ReplyKeyboardMarkup
  = ReplyKeyboardMarkup
      { keyboard :: [KeyboardButtons],
        resize_keyboard :: Bool,
        one_time_keyboard :: Bool,
        selective :: Maybe Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON ReplyKeyboardMarkup where
  toJSON = toJson

instance FromJSON ReplyKeyboardMarkup where
  parseJSON = parseJson
