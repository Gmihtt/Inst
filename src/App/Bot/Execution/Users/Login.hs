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
import Data.Maybe (fromMaybe, maybe)
import Data.String (lines)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified MongoDB.Transforms.InstAccount as Transforms
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Redis.Queries as Redis
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Communication.Scripts.Auth as Auth
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

login :: Message.Message -> Int -> Text -> Flow (Response Message.Message)
login msg userId accLogin = do
  instAccs <- Common.getInstAccs userId
  let res = List.find ((accLogin ==) . InstAccount.login) instAccs
  case res of
    Just _ -> Message.repeatLoggingMsg msg
    Nothing -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.AddAccountPassword accLogin
      Common.updateUserStatus userId status
      Message.passwordMsg msg

password :: Message.Message -> Int -> Text -> Text -> Flow (Response Message.Message)
password msg userId accLogin accPassword = do
  mbRes <- runScript accLogin accPassword
  maybe (errorCase msg userId) (successCase accLogin) mbRes
  where
    successCase accLogin (instId, private, doubleAuth) = do
      if private
        then Message.successAuthMsg msg
        else Message.publicAccount msg
      if doubleAuth
        then do
          let status = TgUserStatus.TgUser $ TgUserStatus.AddAccountCode accLogin accPassword instId
          Common.updateUserStatus userId status
          Message.authCode msg
        else do
          saveAccAndUser instId accLogin accPassword userId
          Message.accountMenu msg
    errorCase msg userId = do
      let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
      Common.updateUserStatus userId status
      Message.failAuthMsg msg
      instAccs <- Common.getInstAccs userId
      Message.showInstAccs msg (map InstAccount.login instAccs)

authCode ::
  Message.Message ->
  Int ->
  Text ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
authCode msg userId instId accLogin accPassword accCode = do
  res <- ScriptsAuth.doubleAuth accLogin accCode
  liftIO $ printDebug res
  if Auth.response_status res
    then do
      Message.successAuthMsg msg
      saveAccAndUser instId accLogin accPassword userId
      Message.accountMenu msg
    else do
      Message.incorrectAuthCode msg

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

saveAccAndUser :: Text -> Text -> Text -> Int -> Flow Bool
saveAccAndUser instId accLogin accPassword userId = do
  instAccs <- Common.getInstAccs userId
  let newInstAcc = InstAccount.mkInstAccount instId accLogin accPassword False
  let uId = T.pack $ show userId
  let tgUser = TgUser.mkTgUser uId (newInstAcc : instAccs)
  Mongo.updateInstAccs uId (Transforms.mkDocByTgUser tgUser) "accounts"
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.putInstAccs userId
  Common.updateUserStatus userId status
