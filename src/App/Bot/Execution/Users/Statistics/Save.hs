module App.Bot.Execution.Users.Statistics.Save where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Communication.Scripts.Statistics.API as StatisticsAPI
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified MongoDB.Queries.Statistics as Mongo
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.InstStatistics as InstStatistics
import qualified Types.Domain.Statistics as Statistics

execute :: Message.Message -> InstAccount.InstId -> Flow ()
execute msg instId = do
  env <- getEnvironment
  let handler = Environment.statisticsMessagesHandler env
  mbStat <- liftIO $ StatisticsAPI.getCurrentStatistics instId handler
  liftIO $ StatisticsAPI.deleteStatistics instId handler
  case mbStat of
    Just eStat -> do
      case eStat of
        Right stat -> do
          finish <- liftIO getCurrentTime
          let count = Statistics.getSize stat
          let statistic = InstStatistics.mkStatistic (fromIntegral count) finish
          let lastCountUsers = Statistics.getLastUsers stat
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
