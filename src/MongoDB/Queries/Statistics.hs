{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.Statistics where

import Common.Flow (Flow)
import Data.Text (Text)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import MongoDB.Queries.Common (callDB)
import qualified MongoDB.Transforms.InstStatistics as Transformer
import qualified Types.Domain.InstStatistics as InstStatistics
import Prelude hiding (id)

updateInstStat :: Text -> InstStatistics.InstStatistics -> Flow ()
updateInstStat id val =
  callDB (Mongo.upsert (Mongo.select ["id" =: id] "statistics") (Transformer.mkDocByInstStatistics val))

findInstStatById :: Text -> Flow (Maybe InstStatistics.InstStatistics)
findInstStatById id = do
  res <- callDB (Mongo.findOne (Mongo.select ["id" =: id] "statistics"))
  pure $ res >>= Transformer.mkInstStatistics

deleteInstStat :: Text -> Flow ()
deleteInstStat id = do
  callDB $ Mongo.deleteOne (Mongo.select ["id" =: id] "statistics")
