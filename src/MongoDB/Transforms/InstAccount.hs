{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.InstAccount
  ( mkDocByInstAcc,
    mkDocsByInstAccs,
    mkInstAccByDoc,
    mkInstAccsByDocs,
  )
where

import Data.Maybe (mapMaybe)
import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import MongoDB.Transforms.Proxy (mkDocByProxy, mkProxyByDoc)
import Types.Domain.InstAccount
  ( InstAccount (..),
    InstAccounts,
  )
import Prelude hiding (id)

mkDocByInstAcc :: InstAccount -> Document
mkDocByInstAcc InstAccount {..} =
  [ "id" =: String id,
    "login" =: String login,
    "password" =: String password,
    "subscription" =: Bool subscription,
    "proxy" =: mkDocByProxy proxy
  ]

mkDocsByInstAccs :: InstAccounts -> [Document]
mkDocsByInstAccs = map mkDocByInstAcc

mkInstAccByDoc :: Document -> Maybe InstAccount
mkInstAccByDoc doc = do
  id <- doc !? "id"
  login <- doc !? "login"
  password <- doc !? "password"
  subscription <- doc !? "subscription"
  proxy <- mkProxyByDoc =<< doc !? "proxy"
  pure InstAccount {..}

mkInstAccsByDocs :: [Document] -> InstAccounts
mkInstAccsByDocs = mapMaybe mkInstAccByDoc
