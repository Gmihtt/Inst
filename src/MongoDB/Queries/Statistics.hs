{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Statistics where

import Common.Flow (Flow)
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import qualified MongoDB.Queries.Common as QMongo
import qualified MongoDB.Transforms.InstStatistics as Transformer
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.InstStatistics as InstStatistics
import Prelude hiding (id)

collectionName :: Text
collectionName = "statistics"

updateInstStat :: InstAccount.InstId -> InstStatistics.InstStatistics -> Flow ()
updateInstStat instId instStat =
  QMongo.upsert (Mongo.select ["id" =: InstAccount.id instId] collectionName) (Transformer.mkDocByInstStatistics instStat)

findInstStatById :: InstAccount.InstId -> Flow (Maybe InstStatistics.InstStatistics)
findInstStatById instId = do
  res <- QMongo.findOne (Mongo.select ["id" =: InstAccount.id instId] collectionName)
  pure $ res >>= Transformer.mkInstStatistics

deleteInstStat :: InstAccount.InstId -> Flow ()
deleteInstStat instId = do
  QMongo.deleteOne (Mongo.select ["id" =: InstAccount.id instId] collectionName)
