module App.Bot.Execution.Users.ShowAccounts where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Telegram.Types.Domain.User as User
import Prelude hiding (id)

selectedAcc :: Message.Message -> User.User -> InstAccount.InstAccount -> Flow (Response Message.Message)
selectedAcc msg user instAcc = do
  let inst_id = InstAccount.id instAcc
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu inst_id
  Common.updateUserStatus user status
  Message.accountMenu msg

addAccount :: Message.Message -> User.User -> Flow (Response Message.Message)
addAccount msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.AddAccountLogin
  Common.updateUserStatus user status
  Message.loginMsg msg

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus user status
  Message.mainMenu msg
