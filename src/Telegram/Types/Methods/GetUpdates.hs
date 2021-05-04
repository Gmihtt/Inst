{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.GetUpdates where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Telegram.Types.Domain.Message as Message

data GetUpdates
  = GetUpdates
      { offset :: Maybe Integer,
        limit :: Maybe Int,
        timeout :: Maybe Int
      }
  deriving (Show, Eq, Generic)

instance ToJSON GetUpdates where
  toJSON = toJson

instance FromJSON GetUpdates where
  parseJSON = parseJson

mkGetUpdates :: Maybe Integer -> Maybe Int -> Maybe Int -> GetUpdates
mkGetUpdates offset limit timeout = GetUpdates {..}
