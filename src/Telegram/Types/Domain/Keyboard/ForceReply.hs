{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types.Domain.Keyboard.ForceReply where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import GHC.Generics (Generic)

data ForceReply
  = ForceReply
      { force_reply :: Bool,
        selective :: Maybe Bool
      }
  deriving (Show, Eq, Generic)

instance ToJSON ForceReply where
  toJSON = toJson

instance FromJSON ForceReply where
  parseJSON = parseJson
