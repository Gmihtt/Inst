{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Admin where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Privilege = High | Medium | Low deriving (Show, Eq, Generic)

instance ToJSON Privilege where
  toJSON = toJson

instance FromJSON Privilege where
  parseJSON = parseJson

data Admin
  = Admin
      { id :: Text,
        privilege :: Privilege
      }
  deriving (Show, Eq)
