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
import Types.Domain.Proxy (Proxy)

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
        timeout :: Maybe Integer,
        proxy :: Maybe Proxy
      }
  deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

mkStartReq :: Text -> Proxy -> Request
mkStartReq inst_id proxy =
  Request
    { status = Start,
      timeout = Just 60000,
      proxy = Just proxy,
      ..
    }

mkStopReq :: Text -> Request
mkStopReq inst_id =
  Request
    { status = Stop,
      timeout = Just 60000,
      proxy = Nothing,
      ..
    }

mkLogoutReq :: Text -> Request
mkLogoutReq inst_id =
  Request
    { status = Logout,
      timeout = Just 60000,
      proxy = Nothing,
      ..
    }
