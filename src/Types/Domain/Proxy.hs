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
import Data.Int(Int64)

data Proxy
  = Proxy
      { ip :: Text,
        port_http :: Int64,
        username :: Text,
        password :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON Proxy where
  parseJSON = parseJson

instance ToJSON Proxy where
  toJSON = toJson
