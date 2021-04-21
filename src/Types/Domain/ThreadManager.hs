module Types.Domain.ThreadManager
  ( module X,
    AuthManager,
    StatisticsManager,
    InfoManager,
  )
where

import Communication.ThreadManager.Manager as X
import qualified Types.Communication.Auth.Response as Auth
import qualified Types.Communication.Info.Response as Info
import qualified Types.Domain.Statistic as Statistic

type AuthManager = Manager Auth.Response

type StatisticsManager = Manager Statistic.Statistic

type InfoManager = Manager Info.Response
