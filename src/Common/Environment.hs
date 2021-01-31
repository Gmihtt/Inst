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
import qualified Types.Domain.Threads as Threads

data Environment
  = Environment
      { manager :: Manager,
        token :: Text,
        pipe :: Pipe,
        conn :: Connection,
        mongoDB :: Text,
        authThreads :: Threads.ThreadsMap ,
        statThreads :: Threads.ThreadsMap
      }

mkEnv :: Manager -> Text -> Pipe -> Connection -> Text -> Threads.ThreadsMap -> Threads.ThreadsMap -> Environment
mkEnv manager token pipe conn mongoDB authThreads statThreads =
  Environment
    { manager = manager,
      token = token,
      pipe = pipe,
      conn = conn,
      mongoDB = mongoDB,
      authThreads = authThreads,
      statThreads = statThreads
    }
