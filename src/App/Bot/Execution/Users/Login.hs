{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Login where

import qualified App.Bot.Messages.FlowMessages as Message
import qualified Common.Environment as Environment
import Common.Error (printDebug, throwLogicError)
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import qualified Communication.Scripts.Auth.API as ScriptsAuth
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified MongoDB.Queries.Usernames as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Auth.Request as RequestAuth
import qualified Types.Communication.Scripts.Auth.Response as ResponseAuth
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Usernames as Usernames
import Prelude hiding (id)

login :: Message.Message -> User.User -> InstAccount.InstUsername -> Flow (Response Message.Message)
login msg user instUsername = do
  instAccs <- Common.getInstAccsByTgUserId (User.id user)
  let res = List.find ((instUsername ==) . InstAccount.instUsername) instAccs
  case res of
    Just _ -> Message.repeatLoggingMsg msg
    Nothing -> do
      Common.setAddAccountPassword user instUsername
      Message.passwordMsg msg

password :: Message.Message -> User.User -> InstAccount.InstUsername -> Text -> Flow (Response Message.Message)
password msg user instUsername accPassword = do
  res <- sendAuthLogin
  liftIO $ printDebug res
  statusHandler msg user instUsername accPassword res
  where
    sendAuthLogin :: Flow (Either Text ResponseAuth.Response)
    sendAuthLogin = do
      env <- getEnvironment
      let authManager = Environment.authMessagesHandler env
      let req = RequestAuth.mkRequestLogin (InstAccount.username instUsername) accPassword
      liftIO $ ScriptsAuth.sendAndReceiveMsg instUsername authManager req

doubleAuth ::
  Message.Message ->
  User.User ->
  InstAccount.InstUsername ->
  Text ->
  Text ->
  Flow (Response Message.Message)
doubleAuth msg user instUsername accPassword accCode = do
  res <- sendDoubleAuth
  liftIO $ printDebug res
  statusHandler msg user instUsername accPassword res
  where
    sendDoubleAuth :: Flow (Either Text ResponseAuth.Response)
    sendDoubleAuth = do
      env <- getEnvironment
      let authManager = Environment.authMessagesHandler env
      let req = RequestAuth.mkRequestDoubleAuth (InstAccount.username instUsername) accCode
      liftIO $ ScriptsAuth.sendAndReceiveMsg instUsername authManager req

sus ::
  Message.Message ->
  User.User ->
  InstAccount.InstUsername ->
  Text ->
  Text ->
  Flow (Response Message.Message)
sus msg user instUsername accPassword accCode = do
  res <- sendSusCode
  liftIO $ printDebug res
  statusHandler msg user instUsername accPassword res
  where
    sendSusCode :: Flow (Either Text ResponseAuth.Response)
    sendSusCode = do
      env <- getEnvironment
      let authManager = Environment.authMessagesHandler env
      let req = RequestAuth.mkRequestSus (InstAccount.username instUsername) accCode
      liftIO $ ScriptsAuth.sendAndReceiveMsg instUsername authManager req

phoneCheck ::
  Message.Message ->
  User.User ->
  InstAccount.InstUsername ->
  Text ->
  Text ->
  Flow (Response Message.Message)
phoneCheck msg user instUsername accPassword accCode = do
  res <- sendPhoneCheck
  liftIO $ printDebug res
  statusHandler msg user instUsername accPassword res
  where
    sendPhoneCheck :: Flow (Either Text ResponseAuth.Response)
    sendPhoneCheck = do
      env <- getEnvironment
      let authManager = Environment.authMessagesHandler env
      let req = RequestAuth.mkRequestPhoneCheck (InstAccount.username instUsername) accCode
      liftIO $ ScriptsAuth.sendAndReceiveMsg instUsername authManager req

statusHandler ::
  Message.Message ->
  User.User ->
  InstAccount.InstUsername ->
  Text ->
  Either Text ResponseAuth.Response ->
  Flow (Response Message.Message)
statusHandler msg user instUsername accPassword eRes = do
  res <- either cricitalCase pure eRes
  case ResponseAuth.status res of
    ResponseAuth.DoubleAuth -> do
      Common.setAddDoubleAuth user instUsername accPassword
      Message.doubleAuth msg
    ResponseAuth.Sus -> do
      Common.setAddSusCode user instUsername accPassword
      Message.susCode msg
    ResponseAuth.PhoneCheck -> do
      Common.setPhoneCheck user instUsername accPassword
      Message.susCode msg
    ResponseAuth.Error -> errorCase
    ResponseAuth.Success ->
      case (ResponseAuth.inst_id res, ResponseAuth.is_private res) of
        (Just instId, Just private) -> do
          if private then Message.successAuthMsg msg else Message.publicAccount msg
          saveAccAndUser (InstAccount.InstId instId) instUsername accPassword user
          Message.accountMenu msg
        _ -> do
          liftIO $ printDebug $
            "For user : " ++ show user
              ++ " with instUsername: "
              ++ show instUsername
              ++ " inst_id or private is Nothing, response: "
              ++ show res
          errorCase
  where
    cricitalCase err = do
      Common.setListOfAccounts user
      Message.smthMessage err msg
      liftIO . throwLogicError $ T.unpack err
    errorCase = do
      Common.setListOfAccounts user
      Message.failAuthMsg msg
      instAccs <- Common.getInstAccsByTgUserId userId
      Message.showInstAccs msg (map InstAccount.instUsername instAccs)
    userId = User.id user

saveAccAndUser :: InstAccount.InstId -> InstAccount.InstUsername -> Text -> User.User -> Flow Bool
saveAccAndUser instId instUsernames accPassword user = do
  let userId = User.id user
  instAccs <- Common.getInstAccsByTgUserId userId
  let newInstAcc = InstAccount.mkInstAccount instId instUsernames accPassword False
  let uId = TgUser.TgId . T.pack $ show userId
  let userFirstName = User.first_name user
  let username = TgUser.TgUsername <$> User.username user
  let tgUser = TgUser.mkTgUser uId userFirstName username (newInstAcc : instAccs)
  Mongo.updateInstAccs uId tgUser
  let newUsernames = Usernames.mkUsernames instUsernames instId username uId
  Mongo.insertUsernames newUsernames
  Common.putInstAccsByTgUserId userId
  Common.setAccountMenu user instId
