{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Telegram.Types.Chat where

import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON(toJSON),
      Value(String),
      genericParseJSON,
      genericToJSON ) 
import Data.Aeson.Casing ( aesonDrop, snakeCase )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (String)

data ChatType = 
  Private | Group | Supergroup | Channel
  deriving (Show, Eq, Generic)

instance ToJSON ChatType where
  toJSON Private = String "private"
  toJSON Group = String "group"
  toJSON Supergroup = String "supergroup"
  toJSON Channel = String "channel"

instance FromJSON ChatType where
  parseJSON (String "private") = pure Private
  parseJSON (String "group") = pure Group
  parseJSON (String "supergroup") = pure Supergroup
  parseJSON (String "channel") = pure Channel

data Chat = Chat {
  chat_id :: Int,
  chat_type :: ChatType,
  chat_title :: Maybe Text,
  chat_username :: Maybe Text,
  chat_first_name :: Maybe Text,
  chat_last_name :: Maybe Text,
  chat_all_members_are_administrators :: Maybe Bool
} deriving (Show, Eq, Generic)

instance ToJSON Chat where
  toJSON = genericToJSON $ aesonDrop 5 snakeCase

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonDrop 5 snakeCase