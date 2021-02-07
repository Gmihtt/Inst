module App.Bot.Execution.Users.Help where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Common.FlowEnv as Common
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

back :: Message.Message -> Int -> Flow (Response Message.Message)
back msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus userId status
  Message.mainMenu msg