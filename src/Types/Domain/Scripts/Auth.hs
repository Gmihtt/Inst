{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Scripts.Auth where

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

data Request
  = Request
      { username :: Text,
        password :: Text
      }
  deriving (Show, Eq, Generic)

mkRequest :: Text -> Text -> Request
mkRequest username password =
  Request
    { username = username,
      password = password
    }

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

data Response
  = Response
      { response_inst_id :: Maybe Text,
        response_username :: Text,
        response_status :: Bool,
        response_errorMessage :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
