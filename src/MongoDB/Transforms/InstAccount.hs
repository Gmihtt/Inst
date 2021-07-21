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
import qualified Types.Domain.InstAccount as InstAccount
import Prelude hiding (id)

mkDocByInstAcc :: InstAccount.InstAccount -> Document
mkDocByInstAcc InstAccount.InstAccount {..} =
  [ "id" =: String (InstAccount.id instId),
    "username" =: String (InstAccount.username instUsername),
    "password" =: String password,
    "subscription" =: Bool subscription
  ]

mkDocsByInstAccs :: InstAccount.InstAccounts -> [Document]
mkDocsByInstAccs = map mkDocByInstAcc

mkInstAccByDoc :: Document -> Maybe InstAccount.InstAccount
mkInstAccByDoc doc = do
  instId <- InstAccount.InstId <$> doc !? "id"
  instUsername <- InstAccount.InstUsername <$> doc !? "username"
  password <- doc !? "password"
  subscription <- doc !? "subscription"
  pure InstAccount.InstAccount {..}

mkInstAccsByDocs :: [Document] -> InstAccount.InstAccounts
mkInstAccsByDocs = mapMaybe mkInstAccByDoc
