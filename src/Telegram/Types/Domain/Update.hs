{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Update
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
import Telegram.Types.Domain.CallbackQuery (CallbackQuery)
import Telegram.Types.Domain.Message (Message)

type Updates = [Update]

data Update
  = Update
      { update_id :: Integer,
        message :: Maybe Message,
        inline_query :: Maybe Value,
        chosen_inline_result :: Maybe Value,
        callback_query :: Maybe CallbackQuery
      }
  deriving (Show, Eq, Generic)

instance ToJSON Update where
  toJSON = toJson

instance FromJSON Update where
  parseJSON = parseJson
