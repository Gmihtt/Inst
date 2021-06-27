module App.Bot.Execution.Admin.AdminMenu where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import qualified Data.Text as T
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

selectUser :: Message.Message -> User.User -> Flow (Response Message.Message)
selectUser msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.SelectTgUser
  Common.updateUserStatus user status
  Message.selectUser msg

selectAdmin :: Message.Message -> User.User -> Flow (Response Message.Message)
selectAdmin msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.SelectAdmin
  Common.updateUserStatus user status
  Message.selectAdmin msg

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus user status
  Message.mainMenu msg
