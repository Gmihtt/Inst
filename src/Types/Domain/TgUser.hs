{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.TgUser
  ( TgUser (..),
    mkTgUser,
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
import Types.Domain.InstAccount (InstAccounts)
import Prelude hiding (id)

data TgUser
  = TgUser
      { id :: Text,
        first_name :: Text,
        username :: Maybe Text,
        inst_accounts :: InstAccounts
      }
  deriving (Show, Eq, Generic)

mkTgUser :: Text -> Text -> Maybe Text -> InstAccounts -> TgUser
mkTgUser id first_name username inst_accounts =
  TgUser {..}

instance ToJSON TgUser where
  toJSON = toJson

instance FromJSON TgUser where
  parseJSON = parseJson
