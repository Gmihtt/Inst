{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Admin where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Privilege = High | Medium | Low deriving (Show, Eq, Generic)

data Admin
  = Admin
      { id :: Text,
        privilege :: Privilege
      }
  deriving (Show, Eq)

mkHighAdmin :: Text -> Admin
mkHighAdmin id =
  Admin
    { id = id,
      privilege = High
    }

mkMediumAdmin :: Text -> Admin
mkMediumAdmin id =
  Admin
    { id = id,
      privilege = Medium
    }

mkLowAdmin :: Text -> Admin
mkLowAdmin id =
  Admin
    { id = id,
      privilege = Low
    }
