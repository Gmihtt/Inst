{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Scripts.Auth where

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

data Auth = Login | DoubleAuth deriving (Show, Eq, Generic)

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

instance ToJSON Request where
  toJSON = toJsonDrop 8

instance FromJSON Request where
  parseJSON = parseJsonDrop 8

data Response
  = Response
      { response_inst_id :: Maybe Text,
        response_username :: Text,
        response_is_double_auth :: Maybe Bool,
        response_is_private :: Maybe Bool,
        response_status :: Bool,
        response_error_message :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
