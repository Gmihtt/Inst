{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.InstAccount
  ( InstAccount (..),
    InstAccounts,
    mkInstAccount,
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
import Types.Domain.Proxy
import Prelude hiding (id)

type InstAccounts = [InstAccount]

data InstAccount
  = InstAccount
      { id :: Text,
        login :: Text,
        password :: Text,
        subscription :: Bool,
        proxy :: Proxy
      }
  deriving (Show, Eq, Generic)

mkInstAccount :: Text -> Text -> Text -> Bool -> Proxy -> InstAccount
mkInstAccount id login password subscription proxy =
  InstAccount {..}

instance ToJSON InstAccount where
  toJSON = toJson

instance FromJSON InstAccount where
  parseJSON = parseJson
