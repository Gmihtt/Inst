{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Update
  ( Updates,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import GHC.Generics (Generic)

import Types.Telegram.Types.Message (Message)

type Updates = [Update]

data Update = Update
  { update_id :: Int,
    message :: Maybe Message,
    inline_query :: Maybe Value,
    chosen_inline_result :: Maybe Value,
    callback_query :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON Update where

instance FromJSON Update where
