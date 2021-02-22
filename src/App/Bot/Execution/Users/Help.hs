module App.Bot.Execution.Users.Help where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import Data.Text (Text)
import qualified Common.TelegramUserStatus as Common
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Telegram.Types.Domain.User as User

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus user status
  Message.mainMenu msg
