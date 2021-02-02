module Common.Environment
  ( Environment (..),
    mkEnv,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Database.MongoDB.Connection (Pipe)
import Database.Redis (Connection)
import Network.HTTP.Client (Manager)
import qualified Types.Domain.Manager as Manager

data Environment
  = Environment
      { manager :: Manager,
        token :: Text,
        pipe :: Pipe,
        conn :: Connection,
        mongoDB :: Text,
        authManager :: Manager.AuthManager,
        statisticsManager :: Manager.StatisticsManager 
      }

mkEnv :: Manager -> Text -> Pipe -> Connection -> Text -> Manager.AuthManager -> Manager.StatisticsManager -> Environment
mkEnv manager token pipe conn mongoDB authThreads statThreads =
  Environment
    { manager = manager,
      token = token,
      pipe = pipe,
      conn = conn,
      mongoDB = mongoDB,
      authManager = authThreads,
      statisticsManager = statThreads
    }
