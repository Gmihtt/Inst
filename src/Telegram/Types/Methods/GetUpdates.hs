{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.GetUpdates where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import GHC.Generics (Generic)

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
