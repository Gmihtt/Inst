{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Transforms.TgUser where

import Database.MongoDB
    ( (=:), Document, Field, Value(..) )
import Types.Domain.TgUser
    ( TgUser(..) )
import MongoDB.Transforms.InstAccount
import Data.Text (pack)
import Prelude hiding (id)

mkDocByTgUser :: TgUser -> Document
mkDocByTgUser tgUser =
  ["id" =: String (id tgUser),
   "inst_accounts" =: mkDocsByInstAccs (inst_accounts tgUser)
  ]