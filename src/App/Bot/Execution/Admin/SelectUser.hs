module App.Bot.Execution.Admin.SelectUser where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Error (throwLogicError)
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUser as TgUser

enterTgUsername :: Message.Message -> User.User -> Flow (Response Message.Message)
enterTgUsername msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.WaitTgUsername
  Common.updateUserStatus user status
  Message.enterUsername msg

selectByTgUsername :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
selectByTgUsername msg user tgUsername = do
  tgUser <-
    Mongo.findTgUserByUsername tgUsername
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  let instAccs = TgUser.inst_accounts tgUser
  let status = TgUserStatus.TgAdmin TgUserStatus.ShowUser
  Common.updateUserStatus user status
  Message.showInstAccs msg (map InstAccount.login instAccs)
  where
    errorMsg =
      "SelectUser.selectByTgUsername fail with tg : "
        ++ show user
        ++ " tgUsername: "
        ++ show tgUsername

enterInstUsername :: Message.Message -> User.User -> Flow (Response Message.Message)
enterInstUsername msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.WaitInstUsername
  Common.updateUserStatus user status
  Message.enterUsername msg

selectByInstUsername :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
selectByInstUsername msg user instUsername = do
  tgUser <-
    Mongo.findTgUserByInstUsername instUsername
      >>= maybe (liftIO $ throwLogicError errorMsg) pure
  let instAccs = TgUser.inst_accounts tgUser
  let status = TgUserStatus.TgAdmin TgUserStatus.ShowUser
  Common.updateUserStatus user status
  Message.showInstAccs msg (map InstAccount.login instAccs)
  where
    errorMsg =
      "SelectUser.selectByInstUsername fail with tg : "
        ++ show user
        ++ " instUsername: "
        ++ show instUsername

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.AdminMenu
  Common.updateUserStatus user status
  Message.adminMenu msg
