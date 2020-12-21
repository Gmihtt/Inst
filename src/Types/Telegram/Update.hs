{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Update
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
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import GHC.Generics (Generic)

type Updates = [Update]

data Update = Update
  { update_id :: Int,
    message :: Maybe Value,
    inline_query :: Maybe Value,
    chosen_inline_result :: Maybe Value,
    callback_query :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON Update where

instance FromJSON Update where
