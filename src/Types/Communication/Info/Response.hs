{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Info.Response where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Status = User | Group | All deriving (Show, Eq, Generic)

instance ToJSON Status where
  toJSON = toJson

instance FromJSON Status where
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
      { status :: Status,
        admin_id :: Text,
        users_info :: Maybe [UserInfo],
        user_count_active :: Maybe Int
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJson

instance FromJSON Response where
  parseJSON = parseJson
