{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Communication.Response
  ( Response (..),
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

data Response a
  = Response
      { ok :: Bool,
        result :: Maybe a,
        description :: Maybe Text,
        error_code :: Maybe Int
      }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Response a) where
  toJSON = toJson

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = parseJson
