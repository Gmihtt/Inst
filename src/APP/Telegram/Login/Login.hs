{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.Login.Login (execute) where

import Common.Flow (Flow)
import qualified APP.Telegram.Messages.Messages as Message
import qualified Types.Telegram.Types.Message as Message
import qualified Types.Telegram.Types.User as User
import Control.Monad.IO.Class ( MonadIO(liftIO) ) 
import qualified Types.Domain.TgUser as TgUser
import qualified MongoDB.Transforms.InstAccount as Transforms
import qualified MongoDB.Transforms.TgUser as Transforms
import qualified Types.Domain.InstAccount as InstAccount
import Types.Telegram.Response (Response (..))
import qualified Redis.Queries as Redis
import qualified MongoDB.Queries as Mongo
import Codec.Binary.UTF8.String ( decode, encode )
import qualified Data.Text as T
import Data.String (lines)
import Data.ByteString (ByteString, pack, unpack)
import Prelude hiding (id)

execute :: Message.Message -> Int -> Flow Message.Message
execute msg userId = do
  userStatus <- getUserStatus
  case userStatus of
    TgUser.Free -> do
      Redis.putValue (packBs userId) (packBs TgUser.WaitAuth)
      pure $ Message.login msg
    TgUser.WaitAuth -> auth msg userId
  where
    getUserStatus = do
      let bUserId = packBs userId
      mbUserStatus <- Redis.getValue bUserId
      pure $ maybe TgUser.Free (TgUser.mkUserStatus . unpackBs) mbUserStatus

login :: Message.Message -> Int -> Flow Message.Message
login msg userId = do
  Redis.putValue (packBs userId) (packBs TgUser.WaitAuth)
  pure $ Message.login msg

auth :: Message.Message -> Int -> Flow Message.Message
auth msg userId = do
  let text = maybe [] lines (T.unpack <$> Message.text msg)
  if length text /= 2 
    then errorCase msg userId
    else do
      let accLogin = head text
      let accPassword = last text
      checkAccStatus msg userId accLogin accPassword

checkAccStatus :: Message.Message -> Int -> String -> String -> Flow Message.Message
checkAccStatus msg userId accLogin accPassword = do
  accStatus <- getAccStatus
  case accStatus of
    InstAccount.Logged -> pure $ Message.repeatLogging msg
    InstAccount.NewAccount -> do
      res <- runScript accLogin accPassword
      if not res
        then errorCase msg userId
        else successCase
  where
    successCase = do
      saveAccAndUser accLogin accPassword userId
      pure $ Message.success msg
    getAccStatus = do
      let bAccLogin = packBs accLogin
      mbAccStatus <- Redis.getValue bAccLogin
      pure $ maybe InstAccount.NewAccount (InstAccount.mkAccountStatus . unpackBs) mbAccStatus

runScript :: String -> String -> Flow Bool
runScript accLogin accPassword = pure True

saveAccAndUser :: String -> String -> Int -> Flow ()
saveAccAndUser accLogin accPassword userId = do
  Redis.putValue (packBs userId) (packBs TgUser.Free)
  Redis.putValue (packBs accLogin) (packBs InstAccount.Logged)
  let uId = T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId uId "accounts"
  let newInstAcc = InstAccount.mkInstAccount (T.pack accLogin) (T.pack accPassword) False
  let tgUser = TgUser.mkTgUser uId (newInstAcc : instAccs)
  Mongo.updateInstAccs uId (Transforms.mkDocByTgUser tgUser) "accounts"

errorCase :: Message.Message -> Int -> Flow Message.Message
errorCase msg userId = do
  Redis.putValue (packBs userId) (packBs TgUser.Free)
  pure $ Message.fail msg

unpackBs :: ByteString -> String
unpackBs = decode . unpack
packBs :: Show a => a -> ByteString
packBs = pack . encode . show