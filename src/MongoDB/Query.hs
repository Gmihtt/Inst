{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Query where

import Database.MongoDB
import Data.Text (Text)
import MongoDB.Transforms.InstAccount ( mkInstAccsByDocs )
import Types.Domain.InstAccount ( InstAccount )
import Prelude hiding (id)

insertDB :: Document -> Pipe -> Database -> Collection -> IO ()
insertDB val pipe db collection = access pipe master db (insert_ collection val)

insetManyDB :: [Document] -> Pipe -> Database -> Collection -> IO ()
insetManyDB val pipe db collection = access pipe master db (insertMany_ collection val)

getSize :: Pipe -> Database -> Collection -> IO Int
getSize pipe db collection = access pipe master db (count (select [] collection))

findInstAccsByTgId :: Text -> Pipe -> Database -> Collection -> IO [InstAccount]
findInstAccsByTgId tg_id pipe db collection = do
  res <- access pipe master db (findOne (select ["id" =: tg_id] collection))
  pure $ maybe [] getInstAccs res
  where
    getInstAccs doc = maybe [] mkInstAccsByDocs (doc !? "inst_accounts")

deleteDB :: Pipe -> Database -> Collection -> IO ()
deleteDB pipe db collection = access pipe master db (delete (select [] collection))


