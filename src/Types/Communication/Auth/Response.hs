{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Auth.Response where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    parseJsonDrop,
    toJson,
    toJsonDrop,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Auth = DoubleAuth | Sus | Success | Error deriving (Show, Eq, Generic)

instance ToJSON Auth where
  toJSON = toJson

instance FromJSON Auth where
  parseJSON = parseJson

data Response
  = Response
      { inst_id :: Maybe Text,
        username :: Text,
        status :: Auth,
        is_private :: Maybe Bool,
        error_message :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
