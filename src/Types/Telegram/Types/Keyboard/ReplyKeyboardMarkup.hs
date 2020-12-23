{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Keyboard.ReplyKeyboardMarkup where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Telegram.Types.Keyboard.KeyboardButton (KeyboardButtons)

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
