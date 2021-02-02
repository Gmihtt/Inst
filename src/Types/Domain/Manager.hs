module Types.Domain.Manager where

import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Domain.Scripts.Auth as Auth
import qualified Types.Domain.Statistic as Statistic
import Types.Domain.Statistics (Statistics)
import qualified Types.Domain.Stream as Stream

type AuthManager = Manager Auth.Response

type StatisticsManager = Manager Statistic.Statistic

data Manager a
  = Manager
      { manager :: Map.Map Text a,
        stream :: Stream.Stream
      }
