{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Transforms.InstAccount
  ( mkDocByInstAcc,
    mkDocsByInstAccs,
    mkInstAccByDoc,
    mkInstAccsByDocs,
  )
where

import Data.Bson ((!?))
import Data.Maybe (mapMaybe)
import Database.MongoDB
  ( (=:),
    Document,
    Value (..),
  )
import Types.Domain.InstAccount
  ( InstAccount (..),
  )
import Prelude hiding (id)

mkDocByInstAcc :: InstAccount -> Document
mkDocByInstAcc instAcc =
  [ "id" =: String (id instAcc),
    "login" =: String (login instAcc),
    "password" =: String (password instAcc),
    "subscription" =: Bool (subscription instAcc)
  ]

mkDocsByInstAccs :: [InstAccount] -> [Document]
mkDocsByInstAccs = map mkDocByInstAcc

mkInstAccByDoc :: Document -> Maybe InstAccount
mkInstAccByDoc doc = do
  inst_id <- doc !? "id"
  inst_login <- doc !? "login"
  inst_password <- doc !? "password"
  inst_subscription <- doc !? "subscription"
  pure
    InstAccount
      { id = inst_id,
        login = inst_login,
        password = inst_password,
        subscription = inst_subscription
      }

mkInstAccsByDocs :: [Document] -> [InstAccount]
mkInstAccsByDocs = mapMaybe mkInstAccByDoc
