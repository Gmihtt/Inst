module App.Bot.Execution.Users.Statistics.Back where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Data.Text (Text)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

backToListAccounts :: Message.Message -> User.User -> Flow (Response Message.Message)
backToListAccounts msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  Common.updateUserStatus user status
  instAccs <- Common.getInstAccs (User.id user)
  Messages.showInstAccs msg (map InstAccount.login instAccs)

backToAccountMenu :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
backToAccountMenu msg user instId = do
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.updateUserStatus user status
  Message.accountMenu msg
