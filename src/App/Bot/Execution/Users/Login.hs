{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Login where

import qualified App.Bot.Messages.FlowMessages as Message
import qualified App.Scripts.Auth.API as ScriptsAuth
import Common.Error (printDebug, throwLogicError)
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified MongoDB.Queries.Usernames as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Auth.Response as ResponseAuth
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Usernames as Usernames
import Prelude hiding (id)

login :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
login msg user accLogin = do
  instAccs <- Common.getInstAccs (User.id user)
  let res = List.find ((accLogin ==) . InstAccount.login) instAccs
  case res of
    Just _ -> Message.repeatLoggingMsg msg
    Nothing -> do
      Common.setAddAccountPassword user accLogin
      Message.passwordMsg msg

password :: Message.Message -> User.User -> Text -> Text -> Flow (Response Message.Message)
password msg user accLogin accPassword = do
  res <- ScriptsAuth.authLogin accLogin accPassword
  liftIO $ printDebug res
  statusHandler msg user accLogin accPassword res

doubleAuth ::
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
doubleAuth msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.doubleAuth accLogin accCode
  liftIO $ printDebug res
  statusHandler msg user accLogin accPassword res

sus ::
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
sus msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.susCode accLogin accCode
  liftIO $ printDebug res
  statusHandler msg user accLogin accPassword res

phoneCheck ::
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
phoneCheck msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.phoneCheck accLogin accCode
  liftIO $ printDebug res
  statusHandler msg user accLogin accPassword res

statusHandler ::
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Either Text ResponseAuth.Response ->
  Flow (Response Message.Message)
statusHandler msg user accLogin accPassword eRes = do
  res <- either cricitalCase pure eRes
  case ResponseAuth.status res of
    ResponseAuth.DoubleAuth -> do
      Common.setAddDoubleAuth user accLogin accPassword
      Message.doubleAuth msg
    ResponseAuth.Sus -> do
      Common.setAddSusCode user accLogin accPassword
      Message.susCode msg
    ResponseAuth.PhoneCheck -> do
      Common.setPhoneCheck user accLogin accPassword
      Message.susCode msg
    ResponseAuth.Error -> errorCase
    ResponseAuth.Success ->
      case (ResponseAuth.inst_id res, ResponseAuth.is_private res) of
        (Just instId, Just private) -> do
          if private then Message.successAuthMsg msg else Message.publicAccount msg
          saveAccAndUser instId accLogin accPassword user
          Message.accountMenu msg
        _ -> do
          liftIO $ printDebug $
            "For user : " ++ show user
              ++ " with accLogin: "
              ++ show accLogin
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
      instAccs <- Common.getInstAccs userId
      Message.showInstAccs msg (map InstAccount.login instAccs)
    userId = User.id user

saveAccAndUser :: Text -> Text -> Text -> User.User -> Flow Bool
saveAccAndUser instId accLogin accPassword user = do
  let userId = User.id user
  instAccs <- Common.getInstAccs userId
  let newInstAcc = InstAccount.mkInstAccount instId accLogin accPassword False
  let uId = T.pack $ show userId
  let userFirstName = User.first_name user
  let username = User.username user
  let tgUser = TgUser.mkTgUser uId userFirstName username (newInstAcc : instAccs)
  Mongo.updateInstAccs uId tgUser
  let newUsernames = Usernames.mkUsernames instId accLogin username uId
  Mongo.insertUsernames newUsernames
  Common.putInstAccs userId
  Common.setAccountMenu user instId
