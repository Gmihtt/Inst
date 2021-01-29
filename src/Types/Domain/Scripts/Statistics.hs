{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Scripts.Statistics where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Request
  = Request
      { id :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON = toJson

instance FromJSON Request where
  parseJSON = parseJson

data Response
  = Response
      { status :: Bool,
        users :: Maybe [Text],
        errorMessage :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJson

instance FromJSON Response where
  parseJSON = parseJson
