{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Statistics where

import Common.Flow (Flow)
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import qualified MongoDB.Queries.Common as QMongo
import qualified MongoDB.Transforms.InstStatistics as Transformer
import qualified Types.Domain.InstStatistics as InstStatistics
import Prelude hiding (id)

collectionName :: Text
collectionName = "statistics"

updateInstStat :: Text -> InstStatistics.InstStatistics -> Flow ()
updateInstStat id instStat =
  QMongo.upsert (Mongo.select ["id" =: id] collectionName) (Transformer.mkDocByInstStatistics instStat)

findInstStatById :: Text -> Flow (Maybe InstStatistics.InstStatistics)
findInstStatById id = do
  res <- QMongo.findOne (Mongo.select ["id" =: id] collectionName)
  pure $ res >>= Transformer.mkInstStatistics

deleteInstStat :: Text -> Flow ()
deleteInstStat id = do
  QMongo.deleteOne (Mongo.select ["id" =: id] collectionName)
