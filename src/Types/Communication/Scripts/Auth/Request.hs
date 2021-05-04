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
import Types.Domain.Proxy (Proxy)

data Auth = Login | DoubleAuth | Sus | PhoneCheck deriving (Show, Eq, Generic)

instance ToJSON Auth where
  toJSON = toJson

instance FromJSON Auth where
  parseJSON = parseJson

data Request
  = Request
      { status :: Auth,
        username :: Text,
        body :: Text,
        proxy :: Maybe Proxy
      }
  deriving (Show, Eq, Generic)

mkRequestLogin :: Text -> Text -> Proxy -> Request
mkRequestLogin username password proxy =
  Request
    { status = Login,
      body = password,
      proxy = Just proxy,
      ..
    }

mkRequestDoubleAuth :: Text -> Text -> Request
mkRequestDoubleAuth inst_id code =
  Request
    { status = DoubleAuth,
      username = inst_id,
      body = code,
      proxy = Nothing
    }

mkRequestSus :: Text -> Text -> Request
mkRequestSus inst_id code =
  Request
    { status = Sus,
      username = inst_id,
      body = code,
      proxy = Nothing
    }

mkRequestPhoneCheck :: Text -> Text -> Request
mkRequestPhoneCheck inst_id code =
  Request
    { status = PhoneCheck,
      username = inst_id,
      body = code,
      proxy = Nothing
    }

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson
