{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.InstStatistics where

import Data.Bson ((!?))
import Data.Maybe (mapMaybe)
import Database.MongoDB
  ( (=:),
    Document,
    Value (..),
  )
import qualified Types.Domain.InstAccount as InstAccount
import Types.Domain.InstStatistics
  ( InstStatistics (..),
    Statistic (..),
    Statistics,
  )
import Prelude hiding (id)

mkDocByStatistic :: Statistic -> Document
mkDocByStatistic Statistic {..} =
  [ "count" =: Int32 count,
    "finish" =: UTC finish
  ]

mkDocsByStatistics :: Statistics -> [Document]
mkDocsByStatistics = map mkDocByStatistic

mkStatistic :: Document -> Maybe Statistic
mkStatistic doc = do
  count <- doc !? "count"
  finish <- doc !? "finish"
  pure Statistic {..}

mkStatistics :: [Document] -> Statistics
mkStatistics = mapMaybe mkStatistic

mkDocByInstStatistics :: InstStatistics -> Document
mkDocByInstStatistics InstStatistics {..} =
  [ "id" =: String (InstAccount.id id),
    "statistics" =: Array (Doc <$> mkDocsByStatistics statistics),
    "lastCountUsers" =: Array (String <$> lastCountUsers)
  ]

mkInstStatistics :: Document -> Maybe InstStatistics
mkInstStatistics doc = do
  id <- InstAccount.InstId <$> doc !? "id"
  stat <- doc !? "statistics"
  lastCountUsers <- doc !? "lastCountUsers"
  let statistics = mkStatistics stat
  pure InstStatistics {..}

mkListOfInstStatistics :: [Document] -> [InstStatistics]
mkListOfInstStatistics = mapMaybe mkInstStatistics
