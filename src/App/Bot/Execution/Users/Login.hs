{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Login
  ( login,
    password,
    doubleAuth,
    sus,
    phoneCheck,
  )
where

import qualified App.Bot.Messages.FlowMessages as Message
import qualified App.Scripts.Auth.API as ScriptsAuth
import qualified Common.Environment as Environment
import Common.Error (printDebug, throwLogicError)
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified MongoDB.Queries.ProxyLoad as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Auth.Response as ResponseAuth
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.ProxyLoad as ProxyLoad
import qualified Types.Domain.ProxyStatus as ProxyStatus
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUser as TgUser
import Prelude hiding (id)

login :: ProxyStatus.ProxyParams -> Message.Message -> User.User -> Text -> Flow (Response Message.Message)
login proxy msg user accLogin = do
  instAccs <- Common.getInstAccs (User.id user)
  let res = List.find ((accLogin ==) . InstAccount.login) instAccs
  case res of
    Just _ -> Message.repeatLoggingMsg msg
    Nothing -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.AddAccountPassword proxy accLogin
      Common.updateUserStatus user status
      Message.passwordMsg msg

password :: ProxyStatus.ProxyParams -> Message.Message -> User.User -> Text -> Text -> Flow (Response Message.Message)
password proxyP msg user accLogin accPassword = do
  let proxy = ProxyStatus.getProxy proxyP
  res <- ScriptsAuth.authLogin accLogin accPassword proxy
  liftIO $ printDebug res
  statusHandler proxyP msg user accLogin accPassword res

doubleAuth ::
  ProxyStatus.ProxyParams ->
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
doubleAuth proxy msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.doubleAuth accLogin accCode
  liftIO $ printDebug res
  statusHandler proxy msg user accLogin accPassword res

sus ::
  ProxyStatus.ProxyParams ->
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
sus proxy msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.susCode accLogin accCode
  liftIO $ printDebug res
  statusHandler proxy msg user accLogin accPassword res

phoneCheck ::
  ProxyStatus.ProxyParams ->
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Text ->
  Flow (Response Message.Message)
phoneCheck proxy msg user accLogin accPassword accCode = do
  res <- ScriptsAuth.phoneCheck accLogin accCode
  liftIO $ printDebug res
  statusHandler proxy msg user accLogin accPassword res

statusHandler ::
  ProxyStatus.ProxyParams ->
  Message.Message ->
  User.User ->
  Text ->
  Text ->
  Either Text ResponseAuth.Response ->
  Flow (Response Message.Message)
statusHandler proxy msg user accLogin accPassword eRes = do
  res <- either cricitalCase pure eRes
  case ResponseAuth.status res of
    ResponseAuth.DoubleAuth -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.AddDoubleAuth proxy accLogin accPassword
      Common.updateUserStatus user status
      Message.doubleAuth msg
    ResponseAuth.Sus -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.AddSusCode proxy accLogin accPassword
      Common.updateUserStatus user status
      Message.susCode msg
    ResponseAuth.PhoneCheck -> do
      let status = TgUserStatus.TgUser $ TgUserStatus.PhoneCheck proxy accLogin accPassword
      Common.updateUserStatus user status
      Message.susCode msg
    ResponseAuth.Error -> errorCase
    ResponseAuth.Success ->
      case (ResponseAuth.inst_id res, ResponseAuth.is_private res) of
        (Just instId, Just private) -> do
          if private then Message.successAuthMsg msg else Message.publicAccount msg
          saveAccAndUser proxy instId accLogin accPassword user
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
      Message.smthMessage err msg
      addProxyTry proxy
      liftIO . throwLogicError $ T.unpack err
    errorCase = do
      let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
      Common.updateUserStatus user status
      Message.failAuthMsg msg
      instAccs <- Common.getInstAccs userId
      addProxyTry proxy
      Message.showInstAccs msg (map InstAccount.login instAccs)
    userId = User.id user

saveAccAndUser :: ProxyStatus.ProxyParams -> Text -> Text -> Text -> User.User -> Flow Bool
saveAccAndUser proxyP instId accLogin accPassword user = do
  let userId = User.id user
  instAccs <- Common.getInstAccs userId
  let proxy = ProxyStatus.getProxy proxyP
  let newInstAcc = InstAccount.mkInstAccount instId accLogin accPassword False proxy
  let uId = T.pack $ show userId
  let userFirstName = User.first_name user
  let username = User.username user
  let tgUser = TgUser.mkTgUser uId userFirstName username (newInstAcc : instAccs)
  Mongo.updateInstAccs uId tgUser
  let status = TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId
  Common.putInstAccs userId
  updateProxyStatus proxyP
  Common.updateUserStatus user status

updateProxyStatus :: ProxyStatus.ProxyParams -> Flow ()
updateProxyStatus proxyP = do
  let proxyLoad = ProxyStatus.proxyLoad proxyP
  let newLoad = ProxyLoad.load proxyLoad + 1
  let newProxyLoad = proxyLoad {ProxyLoad.load = newLoad}
  let newProxyParams = proxyP {ProxyStatus.proxyLoad = newProxyLoad}
  addProxyTry newProxyParams
  Mongo.updateProxyLoad newProxyLoad

addProxyTry :: ProxyStatus.ProxyParams -> Flow ()
addProxyTry proxy = do
  env <- getEnvironment
  let proxyManager = Environment.proxyManager env
  liftIO $ ProxyStatus.addProxyLoad proxy proxyManager
