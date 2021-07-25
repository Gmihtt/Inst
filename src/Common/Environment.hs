module Common.Environment
  ( Environment (..),
  )
where

import Communication.Scripts.Auth.API (AuthMessagesHandler)
import Communication.Scripts.Info.API (InfoMessagesHandler)
import Communication.Scripts.Statistics.API (StatisticsMessagesHandler)
import Data.Text (Text)
import Database.MongoDB.Connection (Pipe)
import Database.Redis (Connection)
import Network.HTTP.Client (Manager)
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

data Environment
  = Environment
      { telegramManager :: Manager,
        token :: Text,
        pipe :: Pipe,
        conn :: Connection,
        mongoDB :: Text,
        collection :: Text,
        authMessagesHandler :: AuthMessagesHandler,
        statisticsMessagesHandler :: StatisticsMessagesHandler,
        infoMessagesHandler :: InfoMessagesHandler,
        tgUsersStatus :: TgUsersStatus.TgUsersStatus,
        logName :: String
      }
