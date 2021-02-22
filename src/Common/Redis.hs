{-# LANGUAGE OverloadedStrings #-}

module Common.Redis where

import qualified App.Bot.Messages.FlowMessages as Message
import qualified Common.Environment as Environment
import Common.Error
import Common.Flow (Flow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import Data.Text (pack)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified Redis.Queries as Redis
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

getInstAccs :: Int -> Flow [InstAccount.InstAccount]
getInstAccs userId = do
  mbInstAccs <- Redis.getValue userId
  maybe (putInstAccs userId >> getInstAccs userId) pure mbInstAccs

putInstAccs :: Int -> Flow ()
putInstAccs userId = do
  let uId = T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId uId "accounts"
  Redis.putValue userId instAccs

dropInstAccs :: Int -> Flow ()
dropInstAccs userId = do
  let uId = T.pack $ show userId
  Redis.deleteValue userId
