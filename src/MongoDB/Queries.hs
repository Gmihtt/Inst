{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


module MongoDB.Queries where

import Database.MongoDB
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Text (Text)
import Common.Flow (Flow)
import qualified Common.Environment as Environment
import MongoDB.Transforms.InstAccount ( mkInstAccsByDocs )
import Types.Domain.InstAccount ( InstAccount )
import Prelude hiding (id)

callDB :: MonadIO m => Action (ReaderT Environment.Environment m) b -> ReaderT Environment.Environment m b
callDB action = do
  env <- ask
  let pipe = Environment.pipe env
  let db = Environment.mongoDB env
  access pipe master db action

insertDB :: Document -> Collection -> Flow ()
insertDB val collection = callDB (insert_ collection val)

insetManyDB :: [Document] -> Collection -> Flow ()
insetManyDB val collection = callDB (insertMany_ collection val)

getSize :: Collection -> Flow Int
getSize collection = callDB (count (select [] collection))

updateInstAccs :: Text -> Document -> Collection -> Flow ()
updateInstAccs tg_id val collection = 
  callDB (replace (select ["id" =: tg_id] collection) val)

findInstAccsByTgId :: Text -> Collection -> Flow [InstAccount]
findInstAccsByTgId tg_id collection = do
  res <- callDB (findOne (select ["id" =: tg_id] collection))
  pure $ maybe [] getInstAccs res
  where
    getInstAccs doc = maybe [] mkInstAccsByDocs (doc !? "inst_accounts")

deleteDB :: Collection -> Flow ()
deleteDB collection = callDB (delete (select [] collection))


