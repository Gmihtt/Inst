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
import Data.Text (pack, unpack)
import Database.MongoDB
  ( (=:),
    Document,
    Field,
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
  id <- doc !? "id"
  login <- doc !? "login"
  password <- doc !? "password"
  subscription <- doc !? "subscription"
  pure
    InstAccount
      { id = id,
        login = login,
        password = password,
        subscription = subscription
      }

mkInstAccsByDocs :: [Document] -> [InstAccount]
mkInstAccsByDocs = mapMaybe mkInstAccByDoc
