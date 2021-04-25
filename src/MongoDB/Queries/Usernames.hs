{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Usernames where

import Common.Flow (Flow)
import qualified Data.List as List
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:), (!?))
import MongoDB.Queries.Common (callDB)
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified MongoDB.Transforms.Usernames as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Usernames as Usernames
import Prelude hiding (id)

insertUsernames :: Text -> TgUser.TgUser -> Flow ()
insertUsernames tgId val = do
  let instUsernames = InstAccount.login <$> TgUser.inst_accounts val
  let tgUsername = TgUser.username val
  let usernames = (\iUn -> Usernames.mkUsernames iUn tgUsername tgId) <$> instUsernames
  callDB (Mongo.insertMany "usernames" $ Transforms.mkDocByUsernames <$> usernames)
  pure ()
  
findUsernamesByInstUsernames :: Text -> Flow (Maybe Usernames.Usernames)
findUsernamesByInstUsernames instUsername = do
  docUsernames <- callDB (Mongo.findOne (Mongo.select ["instUsername" =: instUsername] "usernames"))
  pure $ Transforms.mkUsernamesByDoc =<< docUsernames
  
deleteUsernames :: Text -> Flow ()
deleteUsernames instUsername = do
  callDB $ Mongo.deleteOne (Mongo.select ["instUsername" =: instUsername] "usernames")