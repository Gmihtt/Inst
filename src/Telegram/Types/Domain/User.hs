{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.User
  ( User (..),
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data User
  = User
      { id :: Int,
        first_name :: Text,
        last_name :: Maybe Text,
        username :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON = toJson

instance FromJSON User where
  parseJSON = parseJson
