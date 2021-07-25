{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Domain.InstStatistics where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (diffDays)
import GHC.Generics (Generic)
import qualified Types.Domain.InstAccount as InstAccount
import Prelude hiding (id)

data InstStatistics
  = InstStatistics
      { id :: InstAccount.InstId,
        statistics :: Statistics,
        lastCountUsers :: [Text]
      }
  deriving (Show, Eq, Generic)

instance ToJSON InstStatistics where
  toJSON = toJson

instance FromJSON InstStatistics where
  parseJSON = parseJson

type Statistics = [Statistic]

data Statistic
  = Statistic
      { count :: Int32,
        finish :: UTCTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON Statistic where
  toJSON = toJson

instance FromJSON Statistic where
  parseJSON = parseJson

mkStatistic :: Int32 -> UTCTime -> Statistic
mkStatistic count finish = Statistic {..}

mkInstStatistics :: InstAccount.InstId -> Statistics -> [Text] -> InstStatistics
mkInstStatistics id statistics lastCountUsers =
  InstStatistics
    { ..
    }

addStatistic :: InstStatistics -> Statistic -> InstStatistics
addStatistic InstStatistics {..} stat =
  InstStatistics
    { statistics = stat : statistics,
      ..
    }

groupCountByDay :: UTCTime -> InstStatistics -> Int32
groupCountByDay time InstStatistics {..} =
  countDays time statistics (== 0)

groupCountByWeek :: UTCTime -> InstStatistics -> Int32
groupCountByWeek time InstStatistics {..} =
  countDays time statistics (< 7)

groupCountByMonth :: UTCTime -> InstStatistics -> Int32
groupCountByMonth time InstStatistics {..} =
  countDays time statistics (< 30)

countDays :: UTCTime -> Statistics -> (Integer -> Bool) -> Int32
countDays time stat cmp =
  let filterDays = filter (cmpDay (utctDay time) . utctDay . finish) stat
   in foldr (\a b -> count a + b) 0 filterDays
  where
    cmpDay d1 d2 = cmp $ diffDays d1 d2
