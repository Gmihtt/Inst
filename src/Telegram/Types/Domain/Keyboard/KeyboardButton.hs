{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.KeyboardButton where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type KeyboardButtons = [KeyboardButton]

data KeyboardButton
  = KeyboardButton
      { text :: Text,
        request_contact :: Maybe Bool,
        request_location :: Maybe Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON KeyboardButton where
  toJSON = toJson

instance FromJSON KeyboardButton where
  parseJSON = parseJson
