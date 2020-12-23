{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Update
  ( Updates,
    Update (..),
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    parseJson,
    toJson,
  )
import GHC.Generics (Generic)
import Types.Telegram.Types.Message (Message)

type Updates = [Update]

data Update
  = Update
      { update_id :: Integer,
        message :: Maybe Message,
        inline_query :: Maybe Value,
        chosen_inline_result :: Maybe Value,
        callback_query :: Maybe Value
      }
  deriving (Show, Eq, Generic)

instance ToJSON Update where
  toJSON = toJson

instance FromJSON Update where
  parseJSON = parseJson
