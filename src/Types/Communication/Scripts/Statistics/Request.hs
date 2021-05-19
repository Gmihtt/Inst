{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.Scripts.Statistics.Request where

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
  | Logout
  deriving (Show, Eq, Generic)

instance ToJSON Action where
  toJSON = toJson

instance FromJSON Action where
  parseJSON = parseJson

data Request
  = Request
      { inst_id :: Text,
        status :: Action,
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
    { status = Start,
      timeout = Just 60000,
      ..
    }

mkStopReq :: Text -> Request
mkStopReq inst_id =
  Request
    { status = Stop,
      timeout = Just 60000,
      ..
    }

mkLogoutReq :: Text -> Request
mkLogoutReq inst_id =
  Request
    { status = Logout,
      timeout = Just 60000,
      ..
    }
