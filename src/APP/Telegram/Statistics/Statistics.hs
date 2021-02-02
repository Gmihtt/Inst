{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.Statistics.Statistics where

import qualified APP.Scripts.Statistics.API as API
import qualified APP.Telegram.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import qualified Control.Concurrent.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (pack)
import qualified MongoDB.Queries as QMongo
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.Statistic as Statistic
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.Message as Message

start :: Message.Message -> Int -> Flow (Response Message.Message)
start msg userId = do
  instAccs <- QMongo.findInstAccsByTgId (pack $ show userId) "accounts"
  let instAcc = head instAccs
  env <- ask
  let statManager = Environment.statisticsManager env
  let instAccId = InstAccount.id instAcc
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStartReq instAccId)
  Messages.baseMenu msg

stop :: Message.Message -> Int -> Flow (Response Message.Message)
stop msg userId = do
  instAccs <- QMongo.findInstAccsByTgId (pack $ show userId) "accounts"
  let instAcc = head instAccs
  env <- ask
  let statManager = Environment.statisticsManager env
  let instAccId = InstAccount.id instAcc
  liftIO $ API.sendMsg statManager (ScriptsStat.mkStopReq instAccId)
  Messages.baseMenu msg

stat :: Message.Message -> Int -> Flow (Response Message.Message)
stat msg userId = do
  instAccs <- QMongo.findInstAccsByTgId (pack $ show userId) "accounts"
  if null instAccs
    then Messages.sendEmptyStat msg
    else do
      let instAcc = head instAccs
      env <- ask
      let (Manager.Manager manager stream) = Environment.statisticsManager env
      let instAccId = InstAccount.id instAcc
      mbStat <- liftIO $ Map.lookup instAccId manager
      Messages.sendStat msg $ maybe 0 Statistic.getSize mbStat
