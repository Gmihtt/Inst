{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.Login.Login
  ( execute,
  )
where

import qualified APP.Scripts.Auth.API as ScriptsAuth
import qualified APP.Telegram.Messages.FlowMessages as Message
import Common.Flow (Flow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe, maybe)
import Data.String (lines)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified MongoDB.Transforms.InstAccount as Transforms
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Redis.Queries as Redis
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Scripts.Auth as Auth
import qualified Types.Domain.Status.LoginStatus as LoginStatus
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.UserStatus as UserStatus
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.Message as Message
import qualified Types.Telegram.Types.User as User
import Prelude hiding (id)

execute :: Message.Message -> LoginStatus.LoginStatus -> Int -> Flow (Response Message.Message)
execute msg loginStatus userId = do
  liftIO $ print loginStatus
  case loginStatus of
    LoginStatus.Free -> login msg userId
    LoginStatus.WaitAuth -> auth msg userId

login :: Message.Message -> Int -> Flow (Response Message.Message)
login msg userId = do
  Redis.putValue userId UserStatus.setWaitAuth
  Message.loginMsg msg

auth :: Message.Message -> Int -> Flow (Response Message.Message)
auth msg userId = do
  let text = maybe [] lines (T.unpack <$> Message.text msg)
  if length text /= 2
    then errorCase msg userId
    else do
      let accLogin = head text
      let accPassword = last text
      checkAccStatus msg userId accLogin accPassword

checkAccStatus :: Message.Message -> Int -> String -> String -> Flow (Response Message.Message)
checkAccStatus msg userId accLogin accPassword = do
  accStatus <- getAccStatus
  liftIO $ print accStatus
  case accStatus of
    InstAccount.Logged -> Message.repeatLoggingMsg msg
    InstAccount.NewAccount -> do
      mbRes <- runScript accLogin accPassword
      maybe (errorCase msg userId) successCase mbRes
  where
    successCase instId = do
      saveAccAndUser instId accLogin accPassword userId
      Message.successAuthMsg msg
    getAccStatus = do
      mbAccStatus <- Redis.getValue accLogin
      pure $ maybe InstAccount.NewAccount InstAccount.mkAccountStatus mbAccStatus

runScript :: String -> String -> Flow (Maybe Text)
runScript accLogin accPassword = do
  res <- ScriptsAuth.auth (T.pack accLogin) (T.pack accPassword)
  pure $ Auth.response_inst_id res

saveAccAndUser :: Text -> String -> String -> Int -> Flow ()
saveAccAndUser instId accLogin accPassword userId = do
  Redis.putValue userId UserStatus.setFree
  Redis.putValue accLogin InstAccount.Logged
  let uId = T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId uId "accounts"
  let newInstAcc = InstAccount.mkInstAccount instId (T.pack accLogin) (T.pack accPassword) False
  let tgUser = TgUser.mkTgUser uId (newInstAcc : instAccs)
  Mongo.updateInstAccs uId (Transforms.mkDocByTgUser tgUser) "accounts"

errorCase :: Message.Message -> Int -> Flow (Response Message.Message)
errorCase msg userId = do
  Redis.putValue userId UserStatus.setFree
  Message.failAuthMsg msg
