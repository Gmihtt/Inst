{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.Statistics.Statistics where

import Common.Flow (Flow)
import qualified Types.Telegram.Types.Message as Message
import Types.Telegram.Response (Response (..))
import Data.Text (pack)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Common.Environment as Environment
import qualified APP.Scripts.Statistics.API as API
import qualified Types.Domain.Scripts.Statistics as ScriptsStat
import qualified MongoDB.Queries as QMongo
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Manager as Manager
import qualified Control.Concurrent.Map as Map
import qualified Types.Domain.Statistic as Statistic
import qualified APP.Telegram.Messages.FlowMessages as Messages

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
      
