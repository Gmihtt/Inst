{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.Usernames where

import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import Types.Domain.Usernames (Usernames (..))
import Prelude hiding (id)

mkDocByUsernames :: Usernames -> Document
mkDocByUsernames Usernames {..} =
  [ "instUsername" =: String (InstAccount.username instUsername),
    "instId" =: String (InstAccount.id instId),
    "tgUsername" =: String (maybe "" TgUser.username tgUsername),
    "tgId" =: String (TgUser.id tgId)
  ]

mkUsernamesByDoc :: Document -> Maybe Usernames
mkUsernamesByDoc doc = do
  instUsername <- InstAccount.InstUsername <$> doc !? "instUsername"
  instId <- InstAccount.InstId <$> doc !? "instId"
  let tgUsername = TgUser.TgUsername <$> doc !? "tgUsername"
  tgId <- TgUser.TgId <$> doc !? "tgId"
  pure $ Usernames {..}
