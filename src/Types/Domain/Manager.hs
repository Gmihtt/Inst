module Types.Domain.Manager where

import qualified Types.Domain.Stream as Stream
import qualified Control.Concurrent.Map as Map
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Scripts.Auth as Auth
import Data.ByteString.Lazy (ByteString)
import Types.Domain.Statistics (Statistics)
import Data.Text (Text)

type AuthManager = Manager Auth.Response
type StatisticsManager = Manager Statistic.Statistic 

data Manager a
  = Manager
      { manager :: Map.Map Text a,
        stream :: Stream.Stream
      }