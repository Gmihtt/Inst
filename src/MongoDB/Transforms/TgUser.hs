{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.TgUser where

import Data.Maybe (fromMaybe)
import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import MongoDB.Transforms.InstAccount
import Types.Domain.TgUser
  ( TgUser (..),
  )
import Prelude hiding (id)

mkDocByTgUser :: TgUser -> Document
mkDocByTgUser TgUser {..} =
  [ "id" =: String id,
    "first_name" =: String first_name,
    "username" =: String (fromMaybe "" username),
    "inst_accounts" =: mkDocsByInstAccs inst_accounts
  ]

mkTgUserByDoc :: Document -> Maybe TgUser
mkTgUserByDoc doc = do
  id <- doc !? "id"
  first_name <- doc !? "first_name"
  username <- doc !? "first_name"
  doc_inst_account <- doc !? "inst_accounts"
  let inst_accounts = mkInstAccsByDocs doc_inst_account
  pure $ TgUser {..}
