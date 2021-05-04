{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Accounts where

import Common.Flow (Flow)
import qualified Data.List as List
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import qualified MongoDB.Queries.Common as QMongo
import qualified MongoDB.Queries.Usernames as QUsernames
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Usernames as Usernames
import Prelude hiding (id)

updateInstAccs :: Text -> TgUser.TgUser -> Flow ()
updateInstAccs tgId tgUser = do
  QMongo.upsert (Mongo.select ["id" =: tgId] "accounts") (Transforms.mkDocByTgUser tgUser)
  QUsernames.insertUsernames tgId tgUser

findTgUserById :: Text -> Flow (Maybe TgUser.TgUser)
findTgUserById tg_id = do
  res <- QMongo.findOne (Mongo.select ["id" =: tg_id] "accounts")
  pure $ Transforms.mkTgUserByDoc =<< res

findTgUserByUsername :: Text -> Flow (Maybe TgUser.TgUser)
findTgUserByUsername tgUsername = do
  res <- QMongo.findOne (Mongo.select ["username" =: tgUsername] "accounts")
  pure $ Transforms.mkTgUserByDoc =<< res

findInstAccsByTgId :: Text -> Flow [InstAccount.InstAccount]
findInstAccsByTgId tg_id = do
  tgUser <- findTgUserById tg_id
  pure $ maybe [] TgUser.inst_accounts tgUser

findTgUserByInstUsername :: Text -> Flow (Maybe TgUser.TgUser)
findTgUserByInstUsername instUsername = do
  mbUsernames <- QUsernames.findUsernamesByInstUsernames instUsername
  case Usernames.tgId <$> mbUsernames of
    Nothing -> pure Nothing
    Just tgId -> findTgUserById tgId

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
  QUsernames.deleteUsernames login
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
  QMongo.deleteOne (Mongo.select ["id" =: tg_id] "accounts")
