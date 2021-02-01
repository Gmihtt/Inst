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

type NumOfInstAccounts = Int

data InstAccount
  = InstAccount
      { id :: Text,
        login :: Text,
        password :: Text,
        subscription :: Bool
      }
  deriving (Show, Eq, Generic)

mkInstAccount :: Text -> Text -> Text -> Bool -> InstAccount
mkInstAccount id login password subs =
  InstAccount
    { id = id,
      login = login,
      password = password,
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
