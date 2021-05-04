module App.Bot.Execution.Users.Statistics.Save where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.ThreadManager as Manager

execute :: Message.Message -> Text -> Flow ()
execute msg instId = do
  env <- getEnvironment
  let statManager = Environment.statisticsManager env
  mbStat <- liftIO $ Manager.findTask instId statManager
  liftIO $ Manager.deleteTask instId statManager
  case mbStat of
    Just eStat -> do
      case eStat of
        Right stat -> do
          finish <- liftIO getCurrentTime
          let count = Statistic.getSize stat
          let statistic = InstStatistics.mkStatistic (fromIntegral count) finish
          let lastCountUsers = Statistic.getLastUsers stat
          let newInstStat = InstStatistics.mkInstStatistics instId [statistic] lastCountUsers
          oldInstStat <- Mongo.findInstStatById instId
          let instStat = maybe newInstStat (updateInstStat statistic lastCountUsers) oldInstStat
          Mongo.updateInstStat instId instStat
        Left err -> Messages.smthMessage err msg >> pure ()
    Nothing -> pure ()
  where
    updateInstStat statistic lastCountUsers oldInstStat =
      InstStatistics.addStatistic
        oldInstStat {InstStatistics.lastCountUsers = lastCountUsers}
        statistic
