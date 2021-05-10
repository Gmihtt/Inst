{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Domain.Proxy where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Proxy
  = Proxy
      { ip :: Text,
        port_http :: Int64,
        username :: Text,
        password :: Text
      }
  deriving (Show, Eq, Generic)

testProxySpain :: Proxy
testProxySpain = Proxy {
  ip = "176.118.190.215",
  port_http = 2560,
  username = "user58250",
  password = "5wh9y1"
}

instance FromJSON Proxy where
  parseJSON = parseJson

instance ToJSON Proxy where
  toJSON = toJson
