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

data Environment
  = Environment
      { manager :: Manager,
        token :: Text,
        pipe :: Pipe,
        conn :: Connection,
        mongoDB :: Text
      }

mkEnv :: Manager -> Text -> Pipe -> Connection -> Text -> Environment
mkEnv manager token pipe conn mongoDB =
  Environment
    { manager = manager,
      token = token,
      pipe = pipe,
      conn = conn,
      mongoDB = mongoDB
    }
