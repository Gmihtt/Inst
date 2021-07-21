module App.Bot.Execution.Users.Statistics.Stop where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import qualified Communication.Scripts.Statistics.API as API
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.InstAccount as InstAccount

execute :: Message.Message -> User.User -> InstAccount.InstId -> Flow (Response Message.Message)
execute msg user instId = do
  env <- getEnvironment
  let handler = Environment.statisticsMessagesHandler env
  Common.setAccountMenu user instId
  liftIO $ API.sendMsg handler (RequestStat.mkStopReq (InstAccount.id instId))
  Save.execute msg instId
  Messages.stop msg
