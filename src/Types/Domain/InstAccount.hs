{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.InstAccount where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

type InstAccounts = [InstAccount]

data InstAccount
  = InstAccount
      { instId :: InstId,
        instUsername :: InstUsername,
        password :: Text,
        subscription :: Bool
      }
  deriving (Show, Eq, Generic)

mkInstAccount :: InstId -> InstUsername -> Text -> Bool -> InstAccount
mkInstAccount instId instUsername password subscription =
  InstAccount
    { ..
    }

instance ToJSON InstAccount where
  toJSON = toJson

instance FromJSON InstAccount where
  parseJSON = parseJson

newtype InstId = InstId {id :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Hashable)

newtype InstUsername = InstUsername {username :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Hashable)
