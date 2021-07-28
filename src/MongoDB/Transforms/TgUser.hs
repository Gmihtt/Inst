{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.TgUser where

import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import MongoDB.Transforms.InstAccount
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

mkDocByTgUser :: TgUser.TgUser -> Document
mkDocByTgUser TgUser.TgUser {..} =
  [ "id" =: String (TgUser.id tgId),
    "first_name" =: String first_name,
    "username" =: String (maybe "" TgUser.username tgUsername),
    "inst_accounts" =: mkDocsByInstAccs inst_accounts
  ]

mkTgUserByDoc :: Document -> Maybe TgUser.TgUser
mkTgUserByDoc doc = do
  id <- doc !? "id"
  let tgId = TgUser.TgId id
  first_name <- doc !? "first_name"
  let tgUsername = TgUser.TgUsername <$> doc !? "username"
  doc_inst_account <- doc !? "inst_accounts"
  let inst_accounts = mkInstAccsByDocs doc_inst_account
  pure $ TgUser.TgUser {..}
