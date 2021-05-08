module App.Bot.Execution.Users.Statistics.Stop where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat

execute :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
execute msg user instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  Common.setAccountMenu user instId
  liftIO $ API.sendMsg statManager (RequestStat.mkStopReq instId)
  Save.execute msg instId
  Messages.stop msg
