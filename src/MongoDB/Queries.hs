{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MongoDB.Queries where

import qualified Common.Environment as Environment
import Common.Flow (Flow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:), (!?))
import qualified Data.List as List
import MongoDB.Transforms.InstAccount (mkInstAccsByDocs)
import qualified Types.Domain.InstAccount as InstAccount
import Prelude hiding (id)

callDB :: MonadIO m => Mongo.Action (ReaderT Environment.Environment m) b -> ReaderT Environment.Environment m b
callDB action = do
  env <- ask
  let pipe = Environment.pipe env
  let db = Environment.mongoDB env
  Mongo.access pipe Mongo.master db action

insertDB :: Mongo.Document -> Mongo.Collection -> Flow ()
insertDB val collection = callDB (Mongo.insert_ collection val)

insetManyDB :: [Mongo.Document] -> Mongo.Collection -> Flow ()
insetManyDB val collection = callDB (Mongo.insertMany_ collection val)

getSize :: Mongo.Collection -> Flow Int
getSize collection = callDB (Mongo.count (Mongo.select [] collection))

updateInstAccs :: Text -> Mongo.Document -> Mongo.Collection -> Flow ()
updateInstAccs tg_id val collection =
  callDB (Mongo.upsert (Mongo.select ["id" =: tg_id] collection) val)

findInstAccsByTgId :: Text -> Mongo.Collection -> Flow [InstAccount.InstAccount]
findInstAccsByTgId tg_id collection = do
  res <- callDB (Mongo.findOne (Mongo.select ["id" =: tg_id] collection))
  pure $ maybe [] getInstAccs res
  where
    getInstAccs doc = maybe [] mkInstAccsByDocs (doc !? "inst_accounts")

findInstAccountByLogin :: Text -> Text -> Mongo.Collection -> Flow (Maybe InstAccount.InstAccount)
findInstAccountByLogin tg_id login collection = do
  instAccs <- findInstAccsByTgId tg_id collection
  pure $ List.find ((login ==) . InstAccount.login) instAccs

deleteDB :: Mongo.Collection -> Flow ()
deleteDB collection = callDB (Mongo.delete (Mongo.select [] collection))
