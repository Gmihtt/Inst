{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.Usernames where

import Data.Maybe (fromMaybe)
import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import Types.Domain.Usernames (Usernames (..))
import Prelude hiding (id)

mkDocByUsernames :: Usernames -> Document
mkDocByUsernames Usernames {..} =
  [ "instUsername" =: String instUsername,
    "instId" =: String instId,
    "tgUsername" =: String (fromMaybe "" tgUsername),
    "tgId" =: String tgId
  ]

mkUsernamesByDoc :: Document -> Maybe Usernames
mkUsernamesByDoc doc = do
  instUsername <- doc !? "instUsername"
  instId <- doc !? "instId"
  tgUsername <- doc !? "tgUsername"
  tgId <- doc !? "tgId"
  pure $ Usernames {..}
