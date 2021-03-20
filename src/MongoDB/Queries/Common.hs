module MongoDB.Queries.Common where

import qualified Common.Environment as Environment
import Common.Flow (Flow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Database.MongoDB as Mongo

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

deleteDB :: Mongo.Collection -> Flow ()
deleteDB collection = callDB (Mongo.delete (Mongo.select [] collection))
