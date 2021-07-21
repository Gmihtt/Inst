module Types.Domain.ThreadManager
  ( module X,
    AuthMessagesHandler,
    StatisticsMessagesHandler,
    InfoMessagesHandler,
  )
where

import Types.Domain.MessagesHandler as X
import qualified Types.Communication.Scripts.Auth.Response as Auth
import qualified Types.Communication.Scripts.Error as Error
import qualified Types.Communication.Scripts.Info.Response as Info
import qualified Types.Domain.Statistic as Statistic

type AuthMessagesHandler = MessagesHandler Auth.Response

type StatisticsMessagesHandler = MessagesHandler (Either Error.Error Statistic.Statistic)

type InfoMessagesHandler = MessagesHandler Info.Response
