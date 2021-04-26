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
import Types.Communication.Error ( Error )

data Auth = DoubleAuth | Sus | PhoneCheck | Success | Error deriving (Show, Eq, Generic)

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
        error :: Maybe Error
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJson

instance FromJSON Response where
  parseJSON = parseJson
