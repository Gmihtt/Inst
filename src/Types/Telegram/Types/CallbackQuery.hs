{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.CallbackQuery where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJsonDrop,
    toJsonDrop,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Telegram.Types.Message
import Types.Telegram.Types.User

data CallbackQuery
  = CallbackQuery
      { callback_id :: Text,
        callback_from :: User,
        callback_message :: Maybe Message,
        callback_inline_message_id :: Maybe Text,
        callback_data :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 9

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 9
