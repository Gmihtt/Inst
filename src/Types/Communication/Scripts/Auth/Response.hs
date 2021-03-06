{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Scripts.Auth.Response where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Communication.Scripts.Error (Error)

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
