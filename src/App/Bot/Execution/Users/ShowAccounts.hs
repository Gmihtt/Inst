module App.Bot.Execution.Users.ShowAccounts where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.ProxyStatus as Proxy
import Prelude hiding (id)

selectedAcc :: Message.Message -> User.User -> InstAccount.InstAccount -> Flow (Response Message.Message)
selectedAcc msg user accLogin = do
  let instId = InstAccount.id accLogin
  Common.setAccountMenu user instId
  Message.accountMenu msg

addAccount :: Proxy.ProxyParams -> Message.Message -> User.User -> Flow (Response Message.Message)
addAccount proxy msg user = do
  Common.setAddAccountLogin user proxy
  Message.loginMsg msg

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  Common.setMainMenu user
  Message.mainMenu msg
