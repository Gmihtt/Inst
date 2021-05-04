{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.Scripts.Info.Request where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Status
  = UserStatus
  | GroupStatus
  | AllStatus
  deriving (Show, Eq, Generic)

instance ToJSON Status where
  toJSON = toJson

instance FromJSON Status where
  parseJSON = parseJson

data Request
  = Request
      { inst_ids :: Maybe [Text],
        admin_id :: Text,
        status :: Status
      }
  deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

mkUserStatusReq :: [Text] -> Text -> Request
mkUserStatusReq inst_ids admin_id =
  Request
    { status = UserStatus,
      inst_ids = Just inst_ids,
      ..
    }

mkGroupStatusReq :: [Text] -> Text -> Request
mkGroupStatusReq inst_ids admin_id =
  Request
    { status = GroupStatus,
      inst_ids = Just inst_ids,
      ..
    }

mkAllStatusReq :: Text -> Request
mkAllStatusReq admin_id =
  Request
    { status = AllStatus,
      inst_ids = Nothing,
      ..
    }
