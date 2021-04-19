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

data CountType = Stats | User | Group | All deriving (Show, Eq, Generic)

instance ToJSON CountType where
  toJSON = toJson

instance FromJSON CountType where
  parseJSON = parseJson

data UserInfo
  = UserInfo
      { id :: Text,
        is_active :: Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON UserInfo where
  toJSON = toJson

instance FromJSON UserInfo where
  parseJSON = parseJson

data Response
  = Response
      { response_status :: Bool,
        response_type :: CountType,
        response_user_id :: Text, -- либо inst_id, либо admin_id
        response_users :: Maybe [Text],
        response_users_info :: Maybe [UserInfo],
        user_count_active :: Maybe Int,
        response_errorMessage :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
