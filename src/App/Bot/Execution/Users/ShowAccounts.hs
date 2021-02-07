module App.Bot.Execution.Users.ShowAccounts where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.FlowEnv as Common
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import Prelude hiding (id)

selectedAcc :: Message.Message -> Int -> InstAccount.InstAccount -> Flow (Response Message.Message)
selectedAcc msg userId instAcc = do
  let inst_id = InstAccount.id instAcc
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu inst_id
  Common.updateUserStatus userId status
  Message.accountMenu msg

addAccount :: Message.Message -> Int -> Flow (Response Message.Message)
addAccount msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.AddAccountLogin
  Common.updateUserStatus userId status
  Message.loginMsg msg

back :: Message.Message -> Int -> Flow (Response Message.Message)
back msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus userId status
  Message.mainMenu msg
