{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Transforms.InstAccount
  ( mkDocByInstAcc,
    mkDocsByInstAccs,
    mkInstAccByDoc,
    mkInstAccsByDocs
  )
where

import Data.Text (unpack, pack)
import Database.MongoDB
  ( (=:),
    Document,
    Field,
    Value (..),
  )
import Data.Maybe ( mapMaybe )
import Data.Bson ((!?))
import Types.Domain.InstAccount
  ( InstAccount (..),
  )
import Prelude hiding (id)
  
mkDocByInstAcc :: InstAccount -> Document
mkDocByInstAcc instAcc =
  [ "login" =: String (login instAcc),
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
  pure InstAccount {
        id = id, 
        login = login,
        password = password,
        subscription = subscription
      }

mkInstAccsByDocs :: [Document] -> [InstAccount]
mkInstAccsByDocs = mapMaybe mkInstAccByDoc