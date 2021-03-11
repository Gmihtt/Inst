{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.Scripts.Statistics where

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
        action :: Action,
        timeout :: Maybe Integer
      }
  deriving (Show, Eq, Generic)

mkStartReq :: Text -> Request
mkStartReq inst_id =
  Request
    { action = Start,
      timeout = Just 20000,
      ..
    }

mkStopReq :: Text -> Request
mkStopReq inst_id =
  Request
    { action = Stop,
      timeout = Just 20000,
      ..
    }

mkLogoutReq :: Text -> Request
mkLogoutReq inst_id =
  Request
    { action = Logout,
      timeout = Just 20000,
      ..
    }

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

data Response
  = Response
      { response_inst_id :: Text,
        response_status :: Bool,
        response_users :: Maybe [Text],
        response_errorMessage :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJsonDrop 9

instance FromJSON Response where
  parseJSON = parseJsonDrop 9
