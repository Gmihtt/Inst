{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.TgUser (TgUser(..)) where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Domain.InstAccount ( InstAccount )
import Prelude hiding (id)

data TgUser
  = TgUser
      { id :: Text,
        inst_accounts :: [InstAccount]
      }
  deriving (Show, Eq, Generic)

instance ToJSON TgUser where
  toJSON = toJson

instance FromJSON TgUser where
  parseJSON = parseJson
