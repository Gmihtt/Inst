{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Statistics.Response where

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

data Response
  = Response
      { user_id :: Text,
        status :: Bool,
        users :: Maybe [Text],
        error_message :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJson

instance FromJSON Response where
  parseJSON = parseJson
