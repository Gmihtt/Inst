{-# LANGUAGE OverloadedStrings #-}

module App.Bot.ParseUpdates where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Bot.Selecting.Users.API as UserAPI
import qualified Common.Environment as Environment
import Common.Error (throwTgErr)
import Common.Flow (Flow)
import qualified Common.FlowEnv as Common
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import Telegram.Types.Domain.Update (Update)
import qualified Telegram.Types.Domain.Update as Update
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUpdates as TgUpdates

execute :: TgUpdates.ListOfUpdates -> Environment.Environment -> IO ()
execute updates env = do
  update <- TgUpdates.getUpdate updates
  forkIO $ runReaderT (parseUpdate update) env
  execute updates env

parseUpdate :: Update -> Flow ()
parseUpdate update = do
  case mbCallBack of
    Nothing -> getMsg mbMsg >>= UserAPI.messageFromUser
    Just cb -> maybe (getMsg mbMsg >>= Messages.oldMsg) (checkUserStatus cb) (CallbackQuery.callback_message cb)
  pure ()
  where
    mbCallBack = Update.callback_query update
    mbMsg = Update.message update

checkUserStatus :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
checkUserStatus cb msg = do
  mbStatus <- Common.getUserStatus userId
  case mbStatus of
    Nothing -> do
      Common.updateUserStatus userId (TgUserStatus.TgUser TgUserStatus.MainMenu)
      UserAPI.mainMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.MainMenu) -> UserAPI.mainMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.Help) -> UserAPI.helpMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.ListOfAccounts) -> UserAPI.listOfAccounts cb msg
    Just (TgUserStatus.TgUser (TgUserStatus.AccountMenu instAcc)) -> UserAPI.accountMenu cb msg instAcc
    Just _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from cb

getMsg :: Maybe Message -> Flow Message
getMsg mbMsg = liftIO $ maybe (throwTgErr "Function: parseUpdate. In 'update' field 'message' is Nothing") pure mbMsg
