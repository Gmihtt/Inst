{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Response where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )

import GHC.Generics (Generic)
import Data.Text (Text)

data Response a = Response {
  ok :: Bool,
  result :: Maybe a,
  description :: Maybe Text,
  error_code :: Maybe Int
} deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Response a) where

instance (FromJSON a) => FromJSON (Response a) where
