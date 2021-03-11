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
import Types.Domain.InstAccount (InstAccount)
import Prelude hiding (id)

data TgUser
  = TgUser
      { id :: Text,
        inst_accounts :: [InstAccount]
      }
  deriving (Show, Eq, Generic)

mkTgUser :: Text -> [InstAccount] -> TgUser
mkTgUser id inst_accounts =
  TgUser {..}

instance ToJSON TgUser where
  toJSON = toJson

instance FromJSON TgUser where
  parseJSON = parseJson
