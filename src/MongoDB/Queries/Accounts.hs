{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Accounts where

import Common.Flow (Flow)
import qualified Data.List as List
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((!?), (=:))
import MongoDB.Queries.Common (callDB)
import MongoDB.Transforms.InstAccount (mkInstAccsByDocs)
import qualified MongoDB.Transforms.InstAccount as Transforms
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

updateInstAccs :: Text -> TgUser.TgUser -> Flow ()
updateInstAccs tg_id val =
  callDB (Mongo.upsert (Mongo.select ["id" =: tg_id] "accounts") (Transforms.mkDocByTgUser val))

findInstAccsByTgId :: Text -> Flow [InstAccount.InstAccount]
findInstAccsByTgId tg_id = do
  res <- callDB (Mongo.findOne (Mongo.select ["id" =: tg_id] "accounts"))
  pure $ maybe [] getInstAccs res
  where
    getInstAccs doc = maybe [] mkInstAccsByDocs (doc !? "inst_accounts")

findInstAccountByLogin :: Text -> Text -> Flow (Maybe InstAccount.InstAccount)
findInstAccountByLogin tg_id login = do
  instAccs <- findInstAccsByTgId tg_id
  pure $ List.find ((login ==) . InstAccount.login) instAccs

findInstAccountByInstId :: Text -> Text -> Flow (Maybe InstAccount.InstAccount)
findInstAccountByInstId tg_id instId = do
  instAccs <- findInstAccsByTgId tg_id
  pure $ List.find ((instId ==) . InstAccount.id) instAccs

deleteInstAccount :: Text -> Text -> Flow ()
deleteInstAccount tg_id login = do
  instAccs <- findInstAccsByTgId tg_id
  let newTgUser = TgUser.mkTgUser tg_id (deleteInstAcc instAccs)
  updateInstAccs tg_id newTgUser
  where
    deleteInstAcc [] = []
    deleteInstAcc (instAcc : accs) =
      if InstAccount.login instAcc == login
        then accs
        else instAcc : deleteInstAcc accs

deleteTgUser :: Text -> Flow ()
deleteTgUser tg_id = do
  callDB $ Mongo.deleteOne (Mongo.select ["id" =: tg_id] "accounts")
