{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Usernames where

import Data.Text (Text)
import Prelude hiding (id)

data Usernames
  = Usernames
      { instUsername :: Text,
        instId :: Text,
        tgUsername :: Maybe Text,
        tgId :: Text
      }
  deriving (Show, Eq)

mkUsernames :: Text -> Text -> Maybe Text -> Text -> Usernames
mkUsernames instUsername instId tgUsername tgId =
  Usernames {..}
