module App.SelectingLogic.UsualCases where

import qualified App.Bot.Login.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Common.Flow (Flow)
import qualified Redis.Queries as Redis
import qualified Types.Domain.Status.UserStatus as UserStatus
import Telegram.Types.Communication.Response (Response (..))
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User

execute :: Message -> Flow (Response Message)
execute msg = do
  case Message.from msg of
    Nothing -> Messages.msgForEmptyUser msg
    Just user -> checkStatus msg (User.id user)

checkStatus :: Message -> Int -> Flow (Response Message)
checkStatus msg userId = do
  status <- Redis.getValue userId
  maybe (Messages.baseMenu msg) (choseAction msg userId . UserStatus.mkUserStatus) status

choseAction :: Message -> Int -> UserStatus.UserStatus -> Flow (Response Message)
choseAction msg userId (UserStatus.Login status) = Login.execute msg status userId
choseAction msg _ _ = Messages.baseMenu msg
