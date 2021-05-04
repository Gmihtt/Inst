{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Scripts.Statistics.Response where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Communication.Error (Error)

data Response
  = Response
      { inst_id :: Text,
        users :: Maybe [Text],
        error :: Maybe Error
      }
  deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON = toJson

instance FromJSON Response where
  parseJSON = parseJson
