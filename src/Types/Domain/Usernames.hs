{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Usernames where

import Data.Text (Text)
import Prelude hiding (id)

data Usernames
  = Usernames
      { instUsername :: Text,
        tgUsername :: Maybe Text,
        tgId :: Text
      }
  deriving (Show, Eq)

mkUsernames :: Text -> Maybe Text -> Text -> Usernames
mkUsernames instUsername tgUsername tgId =
  Usernames {..}
