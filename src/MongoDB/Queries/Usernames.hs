{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Usernames where

import Common.Flow (Flow)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import qualified MongoDB.Queries.Common as QMongo
import qualified MongoDB.Transforms.Usernames as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Usernames as Usernames
import Prelude hiding (id)

insertUsernames :: Text -> TgUser.TgUser -> Flow ()
insertUsernames tgId val = do
  let usernames = mkUsernames <$> TgUser.inst_accounts val
  QMongo.insertMany "usernames" $ Transforms.mkDocByUsernames <$> usernames
  pure ()
  where
    mkUsernames instAcc = 
      let tgUsername = TgUser.username val in
      let instId = InstAccount.id instAcc in
      let instUsernames = InstAccount.login instAcc in
      Usernames.mkUsernames instUsernames instId tgUsername tgId

findUsernamesByInstUsernames :: Text -> Flow (Maybe Usernames.Usernames)
findUsernamesByInstUsernames instUsername = do
  docUsernames <- QMongo.findOne (Mongo.select ["instUsername" =: instUsername] "usernames")
  pure $ Transforms.mkUsernamesByDoc =<< docUsernames

getAllUsernames :: Flow [Usernames.Usernames]
getAllUsernames = do
  res <- QMongo.find (Mongo.select [] "proxy_load")
  pure $ mapMaybe Transforms.mkUsernamesByDoc res

deleteUsernames :: Text -> Flow ()
deleteUsernames instUsername = do
  QMongo.deleteOne (Mongo.select ["instUsername" =: instUsername] "usernames")
