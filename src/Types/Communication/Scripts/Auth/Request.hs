{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Scripts.Auth.Request where

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

data Auth = Login | DoubleAuth | Sus deriving (Show, Eq, Generic)

instance ToJSON Auth where
  toJSON = toJson

instance FromJSON Auth where
  parseJSON = parseJson

data Request
  = Request
      { request_type :: Auth,
        request_username :: Text,
        request_body :: Text
      }
  deriving (Show, Eq, Generic)

mkRequestLogin :: Text -> Text -> Request
mkRequestLogin username password =
  Request
    { request_type = Login,
      request_username = username,
      request_body = password
    }

mkRequestDoubleAuth :: Text -> Text -> Request
mkRequestDoubleAuth username password =
  Request
    { request_type = DoubleAuth,
      request_username = username,
      request_body = password
    }

mkRequestSus :: Text -> Text -> Request
mkRequestSus username password =
  Request
    { request_type = Sus,
      request_username = username,
      request_body = password
    }

instance ToJSON Request where
  toJSON = toJsonDrop 8

instance FromJSON Request where
  parseJSON = parseJsonDrop 8
