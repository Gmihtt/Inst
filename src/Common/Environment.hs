{-# LANGUAGE RecordWildCards #-}

module Common.Environment
  ( Environment (..),
    mkEnv,
  )
where

import Data.Text (Text)
import Database.MongoDB.Connection (Pipe)
import Database.Redis (Connection)
import Network.HTTP.Client (Manager)
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Types.Domain.ThreadManager as Manager

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
        infoManager :: Manager.InfoManager,
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
  Manager.InfoManager ->
  TgUsersStatus.TgUsersStatus ->
  String ->
  Environment
mkEnv manager token pipe conn mongoDB collection authManager statisticsManager infoManager tgUsersStatus logName =
  Environment {..}
