{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.InstAccount
  ( InstAccount(..),
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data InstAccount
  = InstAccount
      { id :: Int,
        login :: Text,
        password :: Text,
        subscription :: Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON InstAccount where
  toJSON = toJson

instance FromJSON InstAccount where
  parseJSON = parseJson
