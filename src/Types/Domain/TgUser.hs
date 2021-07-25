{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.TgUser where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Domain.InstAccount (InstAccounts)
import Prelude hiding (id)

data TgUser
  = TgUser
      { tgId :: TgId,
        first_name :: Text,
        tgUsername :: Maybe TgUsername,
        inst_accounts :: InstAccounts
      }
  deriving (Show, Eq, Generic)

mkTgUser :: TgId -> Text -> Maybe TgUsername -> InstAccounts -> TgUser
mkTgUser tgId first_name tgUsername inst_accounts =
  TgUser
    { ..
    }

instance ToJSON TgUser where
  toJSON = toJson

instance FromJSON TgUser where
  parseJSON = parseJson

newtype TgId = TgId {id :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Hashable)

newtype TgUsername = TgUsername {username :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Hashable)
