module App.Bot.Execution.Users.Statistics.Stop where

import qualified App.Bot.Execution.Users.Statistics.Save as Save
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Communication.Statistics.Request as RequestStat

execute :: Message.Message -> Text -> Flow (Response Message.Message)
execute msg instId = do
  env <- ask
  let statManager = Environment.statisticsManager env
  liftIO $ API.sendMsg statManager (RequestStat.mkStopReq instId)
  Save.execute instId
  Messages.stop msg
