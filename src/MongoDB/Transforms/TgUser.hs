{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Transforms.TgUser where

import Data.Text (pack)
import Database.MongoDB
  ( (=:),
    Document,
    Field,
    Value (..),
  )
import MongoDB.Transforms.InstAccount
import Types.Domain.TgUser
  ( TgUser (..),
  )
import Prelude hiding (id)

mkDocByTgUser :: TgUser -> Document
mkDocByTgUser tgUser =
  [ "id" =: String (id tgUser),
    "inst_accounts" =: mkDocsByInstAccs (inst_accounts tgUser)
  ]
