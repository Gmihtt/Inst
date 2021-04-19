{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Scripts.Auth.Response where

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
      { response_inst_id :: Maybe Text,
        response_username :: Text,
        response_type :: Auth,
        response_is_private :: Maybe Bool,
        response_error_message :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
