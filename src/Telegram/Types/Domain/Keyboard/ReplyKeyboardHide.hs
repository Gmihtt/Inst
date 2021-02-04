{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.ReplyKeyboardHide where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data ReplyKeyboardHide
  = ReplyKeyboardHide
      { hide_keyboard :: Bool,
        selective :: Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON ReplyKeyboardHide where
  toJSON = toJson

instance FromJSON ReplyKeyboardHide where
  parseJSON = parseJson
