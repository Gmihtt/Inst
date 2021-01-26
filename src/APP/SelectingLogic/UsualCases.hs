module APP.SelectingLogic.UsualCases where

import Common.Flow (Flow)
import Types.Telegram.Response (Response (..))
import qualified APP.Telegram.Messages.FlowMessages as Messages
import Types.Telegram.Types.Message (Message)
import qualified APP.Telegram.Login.Login as Login
import qualified Redis.Queries as Redis
import qualified Types.Domain.UserStatus as UserStatus
import qualified Types.Telegram.Types.User as User
import qualified Types.Telegram.Types.Message as Message
import APP.Telegram.SendMessage ( sendMessage )

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
choseAction msg userId (UserStatus.Login status ) = Login.execute msg status userId
choseAction msg _ _ = Messages.baseMenu msg