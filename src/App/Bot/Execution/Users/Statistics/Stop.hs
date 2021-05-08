module App.Bot.Execution.Users.Statistics.Stop where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

execute :: Message.Message -> Text -> Flow (Response Message.Message)
execute msg instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  liftIO $ API.sendMsg statManager (RequestStat.mkStopReq instId)
  Save.execute msg instId
  Messages.stop msg
