module Common.Environment
  ( Environment (..),
    mkEnv,
  )
where

import Data.Text (Text)
import Database.MongoDB.Connection (Pipe)
import Database.Redis (Connection)
import Network.HTTP.Client (Manager)
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

data Environment
  = Environment
      { manager :: Manager,
        token :: Text,
        pipe :: Pipe,
        conn :: Connection,
        mongoDB :: Text,
        collection :: Text,
        authManager :: Manager.AuthManager,
        statisticsManager :: Manager.StatisticsManager,
        tgUsersStatus :: TgUsersStatus.TgUsersStatus,
        logName :: String
      }

mkEnv :: 
  Manager -> 
  Text -> 
  Pipe -> 
  Connection -> 
  Text -> 
  Text -> 
  Manager.AuthManager -> 
  Manager.StatisticsManager -> 
  TgUsersStatus.TgUsersStatus ->
  String ->
  Environment
mkEnv e_manager e_token e_pipe e_conn e_mongoDB e_collection e_authThreads e_statThreads e_tgUsersStatus e_logName =
  Environment
    { manager = e_manager,
      token = e_token,
      pipe = e_pipe,
      conn = e_conn,
      mongoDB = e_mongoDB,
      collection = e_collection,
      authManager = e_authThreads,
      statisticsManager = e_statThreads,
      tgUsersStatus = e_tgUsersStatus,
      logName = e_logName
    }
