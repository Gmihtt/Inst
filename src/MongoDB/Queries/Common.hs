module MongoDB.Queries.Common where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Database.MongoDB as Mongo

callDB :: Mongo.Action Flow b -> Flow b
callDB action = do
  env <- getEnvironment
  let pipe = Environment.pipe env
  let db = Environment.mongoDB env
  Mongo.access pipe Mongo.master db action

insertDB :: Mongo.Document -> Mongo.Collection -> Flow ()
insertDB val collection = callDB (Mongo.insert_ collection val)

insetManyDB :: [Mongo.Document] -> Mongo.Collection -> Flow ()
insetManyDB val collection = callDB (Mongo.insertMany_ collection val)

getSize :: Mongo.Collection -> Flow Int
getSize collection = callDB (Mongo.count (Mongo.select [] collection))

deleteDB :: Mongo.Collection -> Flow ()
deleteDB collection = callDB (Mongo.delete (Mongo.select [] collection))
