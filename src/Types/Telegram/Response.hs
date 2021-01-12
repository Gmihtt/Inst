{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Response
  ( Response (..),
    Body (..),
  )
where

import Common.Error (throwTelegramErr)
import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

data Body a
  = Body a
  | Error (Int, Text)
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Body a) where
  toJSON = toJson

instance (FromJSON a) => FromJSON (Body a) where
  parseJSON = parseJson

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
