module App.Bot.Execution.Admin.SelectUser where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import Data.Text ( Text )
import Common.Error (throwLogicError)
import qualified Common.TelegramUserStatus as Common
import qualified MongoDB.Queries.Accounts as Mongo
import Control.Monad.IO.Class (liftIO)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

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
  undefined
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

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.AdminMenu
  Common.updateUserStatus user status
  Message.adminMenu msg