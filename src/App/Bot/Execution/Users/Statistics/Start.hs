{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.Statistics.Start where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
import qualified Common.Environment as Environment
import Common.Error (throwLogicError)
import Common.Flow (Flow, getEnvironment)
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified MongoDB.Queries.Statistics as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.ThreadManager as Manager

checkStart :: Message.Message -> User.User -> Text -> Flow (Response Message.Message)
checkStart msg user instId = do
  Common.setWaitStart user instId
  Messages.continueStat msg

start :: Bool -> Message.Message -> User.User -> Text -> Flow (Response Message.Message)
start False msg user instId = do
  sendStartMsg user instId
  Common.setAccountMenu user instId
  Messages.start msg
start True msg user instId = do
  env <- getEnvironment
  mbInstStat <- Mongo.findInstStatById instId
  case mbInstStat of
    Just instStat -> do
      let statManager = Environment.statisticsManager env
      sendStartMsg user instId
      let statistics = Right . Statistic.initWithLastUsers $ InstStatistics.lastCountUsers instStat
      liftIO $ Manager.addTask instId statistics statManager
      Common.setAccountMenu user instId
      Messages.start msg
    Nothing -> do
      Messages.lastCountUsersNotFound msg
      start False msg user instId

sendStartMsg :: User.User -> Text -> Flow ()
sendStartMsg user instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  let userId = User.id user
  let tgId = T.pack $ show userId
  instAccount <-
    Mongo.findInstAccountByInstId tgId instId
      >>= maybe (liftIO $ throwLogicError errMsg) pure
  let proxy = InstAccount.proxy instAccount
  liftIO $ API.sendMsg statManager (RequestStat.mkStartReq instId proxy)
  where
    errMsg = "Start.start instAccount don't find by instId: " ++ T.unpack instId ++ " for user: " ++ show user
