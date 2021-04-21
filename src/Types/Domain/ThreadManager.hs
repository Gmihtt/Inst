module Types.Domain.ThreadManager (module X, AuthManager, StatisticsManager) where

import Communication.ThreadManager.Manager as X
import qualified Types.Communication.Scripts.Auth.Response as Auth
import qualified Types.Domain.Statistic as Statistic

type AuthManager = Manager Auth.Response

type StatisticsManager = Manager Statistic.Statistic
