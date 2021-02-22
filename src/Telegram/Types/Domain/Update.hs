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
        edited_message :: Maybe Message,
        channel_post :: Maybe Value,
        edited_channel_post :: Maybe Value,
        shipping_query :: Maybe Value,
        pre_checkout_query :: Maybe Value,
        poll :: Maybe Value,
        poll_answer :: Maybe Value,
        inline_query :: Maybe Value,
        chosen_inline_result :: Maybe Value,
        callback_query :: Maybe CallbackQuery
      }
  deriving (Show, Eq, Generic)

instance ToJSON Update where
  toJSON = toJson

instance FromJSON Update where
  parseJSON = parseJson
