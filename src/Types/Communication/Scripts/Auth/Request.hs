{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.Scripts.Auth.Request where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Auth = Login | DoubleAuth | Sus | PhoneCheck deriving (Show, Eq, Generic)

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

mkRequestPhoneCheck :: Text -> Text -> Request
mkRequestPhoneCheck inst_id code =
  Request
    { status = PhoneCheck,
      username = inst_id,
      body = code
    }

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson
