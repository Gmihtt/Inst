module Types.Domain.Statistics where

import qualified Control.Concurrent.Map as Map
import Data.Text (Text)
import qualified Types.Domain.Statistic as Statistic

type Users = [Text]

type Statistics = Map.Map Text Statistic.Statistic

initStatistic :: IO Statistics
initStatistic = Map.empty

addUsers :: Text -> Users -> Statistics -> IO Statistics
addUsers username users statistics = undefined
