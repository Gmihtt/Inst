{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Statistics.Request where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Action
  = Start
  | Stop
  | UserStatus
  | GroupStatus
  | AllStatus
  | Logout
  deriving (Show, Eq, Generic)

instance ToJSON Action where
  toJSON = toJson

instance FromJSON Action where
  parseJSON = parseJson

data Request
  = Request
      { inst_ids :: Maybe [Text],
        action :: Action,
        timeout :: Maybe Integer
      }
  deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

mkStartReq :: Text -> Request
mkStartReq inst_id =
  Request
    { action = Start,
      timeout = Just 20000,
      inst_ids = Just [inst_id]
    }

mkStopReq :: Text -> Request
mkStopReq inst_id =
  Request
    { action = Stop,
      timeout = Just 20000,
      inst_ids = Just [inst_id]
    }

mkLogoutReq :: Text -> Request
mkLogoutReq inst_id =
  Request
    { action = Logout,
      timeout = Just 20000,
      inst_ids = Just [inst_id]
    }

mkUserStatusReq :: Text -> Request
mkUserStatusReq inst_id =
  Request
    { action = UserStatus,
      timeout = Just 20000,
      inst_ids = Just [inst_id]
    }

mkGroupStatusReq :: Text -> Request
mkGroupStatusReq ids =
  Request
    { action = GroupStatus,
      timeout = Just 20000,
      inst_ids = Just [ids]
    }

mkAllStatusReq :: Request
mkAllStatusReq =
  Request
    { action = AllStatus,
      timeout = Just 20000,
      inst_ids = Nothing
    }
