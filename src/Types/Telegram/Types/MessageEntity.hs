{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.MessageEntity (MessageEntities) where

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

type MessageEntities = [MessageEntity]

data MessageEntity = MessageEntity {
  entity_type :: Text,
  entity_offset :: Int,
  entity_length :: Int,
  entity_url :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON MessageEntity where
  toJSON = genericToJSON $ aesonDrop 7 snakeCase

instance FromJSON MessageEntity where
  parseJSON = genericParseJSON $ aesonDrop 7 snakeCase