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

backToListAccounts :: Message.Message -> User.User -> Flow (Response Message.Message)
backToListAccounts msg user = do
  Common.setListOfAccounts user
  instAccs <- Common.getInstAccsByTgUserId (User.id user)
  Messages.showInstAccs msg (map InstAccount.instUsername instAccs)

backToAccountMenu :: Message.Message -> User.User -> InstAccount.InstId -> Flow (Response Message.Message)
backToAccountMenu msg user instId = do
  Common.setAccountMenu user instId
  Message.accountMenu msg
