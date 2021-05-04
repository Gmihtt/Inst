{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Proxy where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Proxy
  = Proxy
      { ip :: Text,
        port_http :: Text,
        username :: Text,
        password :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON Proxy where
  parseJSON = parseJson

instance ToJSON Proxy where
  toJSON = toJson
