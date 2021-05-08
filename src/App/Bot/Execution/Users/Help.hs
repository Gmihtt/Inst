module App.Bot.Execution.Users.Help where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  Common.setMainMenu user
  Message.mainMenu msg
