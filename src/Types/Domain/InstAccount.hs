{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.InstAccount
  ( InstAccount (..),
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

data InstAccount
  = InstAccount
      { id :: Text,
        login :: Text,
        password :: Text,
        subscription :: Bool
      }
  deriving (Show, Eq, Generic)

mkInstAccount :: Text -> Text -> Text -> Bool -> InstAccount
mkInstAccount inst_id inst_login inst_password subs =
  InstAccount
    { id = inst_id,
      login = inst_login,
      password = inst_password,
      subscription = subs
    }

instance ToJSON InstAccount where
  toJSON = toJson

instance FromJSON InstAccount where
  parseJSON = parseJson

data AccountStatus = NewAccount | Logged
  deriving (Show)

mkAccountStatus :: String -> AccountStatus
mkAccountStatus "Logged" = Logged
mkAccountStatus _ = NewAccount
