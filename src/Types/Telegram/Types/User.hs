{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.User (User) where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
  { id :: Int,
    first_name :: Text,
    last_name :: Maybe Text,
    username :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON User

instance FromJSON User