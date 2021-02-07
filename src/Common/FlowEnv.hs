{-# LANGUAGE OverloadedStrings #-}

module Common.FlowEnv where

import Common.Flow (Flow)
import Data.Text (pack)
import qualified Data.Text as T
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Telegram.Types.Communication.Response (Response (..))
import qualified Redis.Queries as Redis
import qualified MongoDB.Queries as Mongo
import qualified App.Bot.Messages.FlowMessages as Message
import qualified Common.Environment as Environment
import qualified Types.Domain.InstAccount as InstAccount
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

updateUserStatus :: Int -> TgUserStatus.TgUserStatus -> Flow Bool
updateUserStatus userId status = do
  env <- ask
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show userId
  liftIO  $ TgUsersStatus.updateUserStatus uId status tgUsersStatus

getUserStatus :: Int -> Flow (Maybe TgUserStatus.TgUserStatus)
getUserStatus userId = do
  env <- ask
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show userId
  liftIO $ TgUsersStatus.getUserStatus uId tgUsersStatus

getInstAccs :: Int -> Flow [InstAccount.InstAccount]
getInstAccs userId = do
  mbInstAccs <- Redis.getValue userId
  maybe (putInstAccs userId >> getInstAccs userId) pure mbInstAccs

putInstAccs :: Int -> Flow ()
putInstAccs userId = do
  let uId = T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId uId "accounts"
  Redis.putValue userId instAccs