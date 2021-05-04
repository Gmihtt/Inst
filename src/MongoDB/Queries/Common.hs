module MongoDB.Queries.Common where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Common.Transforms (fromObjectIdToId)
import Data.Bson
import Data.Text
import qualified Database.MongoDB as Mongo

callDB :: Mongo.Action Flow b -> Flow b
callDB action = do
  env <- getEnvironment
  let pipe = Environment.pipe env
  let db = Environment.mongoDB env
  Mongo.access pipe Mongo.master db action

insert :: Mongo.Collection -> Mongo.Document -> Flow (Maybe Text)
insert collection doc = do
  res <- cast' <$> callDB (Mongo.insert collection doc)
  pure $ fromObjectIdToId <$> res

upsert :: Mongo.Selection -> Mongo.Document -> Flow ()
upsert selection doc = callDB (Mongo.upsert selection doc)

insertMany :: Mongo.Collection -> [Mongo.Document] -> Flow ()
insertMany collection doc = callDB (Mongo.insertMany_ collection doc)

findOne :: Mongo.Query -> Flow (Maybe Mongo.Document)
findOne query = callDB (Mongo.findOne query)

find :: Mongo.Query -> Flow [Mongo.Document]
find query = callDB (Mongo.rest =<< Mongo.find query)

deleteOne :: Mongo.Selection -> Flow ()
deleteOne selection =
  callDB $ Mongo.deleteOne selection

deleteAll :: Mongo.Collection -> Flow ()
deleteAll collection = callDB (Mongo.delete (Mongo.select [] collection))

getSize :: Mongo.Collection -> Flow Int
getSize collection = callDB (Mongo.count (Mongo.select [] collection))
