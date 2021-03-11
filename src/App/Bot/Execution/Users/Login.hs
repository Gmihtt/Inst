{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Login
  ( login,
    password,
    authCode,
  )
where

import qualified App.Bot.Messages.FlowMessages as Message
import qualified App.Scripts.Auth.API as ScriptsAuth
import Common.Error (printDebug)
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified MongoDB.Transforms.TgUser as Transforms
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Auth as Auth
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

login :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
login msg user accLogin = do
  instAccs <- Common.getInstAccs (User.id user)
  let res = List.find ((accLogin ==) . InstAccount.login) instAccs
  case res of
    Just _ -> Message.repeatLoggingMsg msg
    Nothing -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.AddAccountPassword accLogin
      Common.updateUserStatus user status
      Message.passwordMsg msg

password :: Message.Message -> User.User -> Text -> Text -> Flow (Response Message.Message)
password msg user accLogin accPassword = do
  mbRes <- runScript accLogin accPassword
  maybe errorCase successCase mbRes
  where
    successCase (instId, private, doubleAuth) = do
      if private
        then Message.successAuthMsg msg
        else Message.publicAccount msg
      if doubleAuth
        then do
          let status = TgUserStatus.TgUser $ TgUserStatus.AddAccountCode accLogin accPassword instId
          Common.updateUserStatus user status
          Message.authCode msg
        else do
          saveAccAndUser instId accLogin accPassword user
          Message.accountMenu msg
    errorCase = do
      let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
      Common.updateUserStatus user status
      Message.failAuthMsg msg
      instAccs <- Common.getInstAccs userId
      Message.showInstAccs msg (map InstAccount.login instAccs)
    userId = User.id user

authCode ::
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
authCode msg user instId accLogin accPassword accCode = do
  res <- ScriptsAuth.doubleAuth accLogin accCode
  liftIO $ printDebug res
  if Auth.response_status res
    then do
      Message.successAuthMsg msg
      saveAccAndUser instId accLogin accPassword user
      Message.accountMenu msg
    else do
      Message.incorrectAuthCode msg
      let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
      Common.updateUserStatus user status
      instAccs <- Common.getInstAccs (User.id user)
      Message.showInstAccs msg (map InstAccount.login instAccs)

runScript :: Text -> Text -> Flow (Maybe (Text, Bool, Bool))
runScript accLogin accPassword = do
  res <- ScriptsAuth.authLogin accLogin accPassword
  liftIO $ printDebug res
  let mbInstId = Auth.response_inst_id res
  let mbPrivate = Auth.response_is_private res
  let mbDoubleAuth = Auth.response_is_double_auth res
  pure $ mkRes mbInstId mbPrivate mbDoubleAuth
  where
    mkRes mbInstId mbPrivate mbDoubleAuth = do
      instId <- mbInstId
      private <- mbPrivate
      doubleAuth <- mbDoubleAuth
      pure (instId, private, doubleAuth)

saveAccAndUser :: Text -> Text -> Text -> User.User -> Flow Bool
saveAccAndUser instId accLogin accPassword user = do
  let userId = User.id user
  instAccs <- Common.getInstAccs userId
  let newInstAcc = InstAccount.mkInstAccount instId accLogin accPassword False
  let uId = T.pack $ show userId
  let tgUser = TgUser.mkTgUser uId (newInstAcc : instAccs)
  Mongo.updateInstAccs uId (Transforms.mkDocByTgUser tgUser) "accounts"
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.putInstAccs userId
  Common.updateUserStatus user status
