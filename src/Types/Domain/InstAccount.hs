{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.InstAccount
  ( InstAccount (..),
    InstAccounts,
    AccountStatus (..),
    mkInstAccount,
    mkAccountStatus,
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
import Prelude hiding (id)

type InstAccounts = [InstAccount]

data InstAccount
  = InstAccount
      { id :: Text,
        login :: Text,
        password :: Text,
        subscription :: Bool
      }
  deriving (Show, Eq, Generic)

mkInstAccount :: Text -> Text -> Text -> Bool -> InstAccount
mkInstAccount id login password subscription =
  InstAccount {..}

instance ToJSON InstAccount where
  toJSON = toJson

instance FromJSON InstAccount where
  parseJSON = parseJson

data AccountStatus = NewAccount | Logged
  deriving (Show)

mkAccountStatus :: String -> AccountStatus
mkAccountStatus "Logged" = Logged
mkAccountStatus _ = NewAccount
