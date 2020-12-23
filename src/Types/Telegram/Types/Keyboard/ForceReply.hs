{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Keyboard.ForceReply where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
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
