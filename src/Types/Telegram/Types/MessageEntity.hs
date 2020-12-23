{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.MessageEntity
  ( MessageEntities,
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJsonDrop,
    toJsonDrop,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (String)

type MessageEntities = [MessageEntity]

data MessageEntity
  = MessageEntity
      { entity_type :: Text,
        entity_offset :: Int,
        entity_length :: Int,
        entity_url :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON MessageEntity where
  toJSON = toJsonDrop 7

instance FromJSON MessageEntity where
  parseJSON = parseJsonDrop 7
