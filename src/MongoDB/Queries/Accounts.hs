{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Accounts where

import Common.Flow (Flow)
import qualified Data.List as List
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import MongoDB.Queries.Common (callDB)
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

updateInstAccs :: Text -> TgUser.TgUser -> Flow ()
updateInstAccs tg_id val =
  callDB (Mongo.upsert (Mongo.select ["id" =: tg_id] "accounts") (Transforms.mkDocByTgUser val))

findTgUserById :: Text -> Flow (Maybe TgUser.TgUser)
findTgUserById tg_id = do
  res <- callDB (Mongo.findOne (Mongo.select ["id" =: tg_id] "accounts"))
  pure $ Transforms.mkTgUserByDoc =<< res

findInstAccsByTgId :: Text -> Flow [InstAccount.InstAccount]
findInstAccsByTgId tg_id = do
  tgUser <- findTgUserById tg_id
  pure $ maybe [] TgUser.inst_accounts tgUser

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
  mbTgUser <- findTgUserById tg_id
  case mbTgUser of
    Nothing -> pure ()
    Just tgUser -> do
      let instAccs = TgUser.inst_accounts tgUser
      let newTgUser = tgUser {TgUser.inst_accounts = deleteInstAcc instAccs}
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
