{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.Auth.Request where

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

data Auth = Login | DoubleAuth | Sus deriving (Show, Eq, Generic)

instance ToJSON Auth where
  toJSON = toJson

instance FromJSON Auth where
  parseJSON = parseJson

data Request
  = Request
      { status :: Auth,
        username :: Text,
        body :: Text
      }
  deriving (Show, Eq, Generic)

mkRequestLogin :: Text -> Text -> Request
mkRequestLogin username password =
  Request
    { status = Login,
      body = password,
      ..
    }

mkRequestDoubleAuth :: Text -> Text -> Request
mkRequestDoubleAuth inst_id code =
  Request
    { status = DoubleAuth,
      username = inst_id,
      body = code
    }

mkRequestSus :: Text -> Text -> Request
mkRequestSus inst_id code =
  Request
    { status = Sus,
      username = inst_id,
      body = code
    }

instance ToJSON Request where
  toJSON = toJsonDrop 8

instance FromJSON Request where
  parseJSON = parseJsonDrop 8
