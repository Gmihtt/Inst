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

collectionName :: Text
collectionName = "accounts"

updateInstAccs :: TgUser.TgId -> TgUser.TgUser -> Flow ()
updateInstAccs tgId tgUser = do
  QMongo.upsert (Mongo.select ["id" =: TgUser.id tgId] collectionName) (Transforms.mkDocByTgUser tgUser)

findTgUserById :: TgUser.TgId -> Flow (Maybe TgUser.TgUser)
findTgUserById tgId = do
  res <- QMongo.findOne (Mongo.select ["id" =: TgUser.id tgId] collectionName)
  pure $ Transforms.mkTgUserByDoc =<< res

findTgUserByUsername :: TgUser.TgUsername -> Flow (Maybe TgUser.TgUser)
findTgUserByUsername tgUsername = do
  res <- QMongo.findOne (Mongo.select ["username" =: TgUser.username tgUsername] collectionName)
  pure $ Transforms.mkTgUserByDoc =<< res

findInstAccsByTgId :: TgUser.TgId -> Flow [InstAccount.InstAccount]
findInstAccsByTgId tgId = do
  tgUser <- findTgUserById tgId
  pure $ maybe [] TgUser.inst_accounts tgUser

findTgUserByInstUsername :: InstAccount.InstUsername -> Flow (Maybe TgUser.TgUser)
findTgUserByInstUsername username = do
  mbUsernames <- QUsernames.findUsernamesByInstUsernames username
  case Usernames.tgId <$> mbUsernames of
    Nothing -> pure Nothing
    Just tgId -> findTgUserById tgId

findInstAccountByLogin :: TgUser.TgId -> InstAccount.InstUsername -> Flow (Maybe InstAccount.InstAccount)
findInstAccountByLogin tgId instUsername = do
  instAccs <- findInstAccsByTgId tgId
  pure $ List.find ((instUsername ==) . InstAccount.instUsername) instAccs

findInstAccountByInstId :: TgUser.TgId -> InstAccount.InstId -> Flow (Maybe InstAccount.InstAccount)
findInstAccountByInstId tgId instId = do
  instAccs <- findInstAccsByTgId tgId
  pure $ List.find ((instId ==) . InstAccount.instId) instAccs

deleteInstAccount :: TgUser.TgId -> InstAccount.InstUsername -> Flow ()
deleteInstAccount tgId username = do
  mbTgUser <- findTgUserById tgId
  QUsernames.deleteUsernames username
  case mbTgUser of
    Nothing -> pure ()
    Just tgUser -> do
      let instAccs = TgUser.inst_accounts tgUser
      let newTgUser = tgUser {TgUser.inst_accounts = deleteInstAcc instAccs}
      updateInstAccs tgId newTgUser
  where
    deleteInstAcc [] = []
    deleteInstAcc (instAcc : accs) =
      if InstAccount.instUsername instAcc == username
        then accs
        else instAcc : deleteInstAcc accs

deleteTgUser :: TgUser.TgId -> Flow ()
deleteTgUser tgId = do
  QMongo.deleteOne (Mongo.select ["id" =: TgUser.id tgId] collectionName)
