module App.App (app) where

import App.Bot.BotMain (run)
import qualified App.Scripts.Auth.API as ScriptsAuth
import qualified App.Scripts.Statistics.API as ScriptsStatistics
import qualified Common.Config as Config
import Common.Environment (mkEnv)
import Common.Error (Error (..))
import Control.Concurrent (forkIO)
import Control.Exception (Exception, SomeException, catch)
import Control.Monad.Trans.Except (runExceptT)
import qualified Database.MongoDB as MongoDB
import qualified Database.Redis as Redis
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import qualified System.Log.Handler.Simple as Logger
import qualified System.Log.Handler.Syslog as Logger
import qualified System.Log.Logger as Logger
import qualified System.Process as System
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

setCommonFormatter :: LogHandler a => a -> a
setCommonFormatter x =
  let f = simpleLogFormatter "$utcTime $prio $loggername: $msg"
   in setFormatter x f

app :: IO ()
app =
  catch
    ( do
        manager <- newManager tlsManagerSettings
        token <- Config.getToken
        pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
        conn <- Redis.checkedConnect Redis.defaultConnectInfo
        mongoDB <- Config.getDataBase
        collection <- Config.getCollection
        authSocket <- Config.getAuthSocket
        statSocket <- Config.getStatSocket
        authManager <- ScriptsAuth.authConnection authSocket
        statManager <- ScriptsStatistics.statConnection statSocket
        tgUsersStatus <- TgUsersStatus.empty
        let logName = "BotLogger.Main"
        logger logName
        let env = mkEnv manager token pipe conn mongoDB collection authManager statManager tgUsersStatus logName
        print "App running..."
        run Nothing env
        MongoDB.close pipe
    )
    handler
  where
    logger logName = do
      fh <- Logger.fileHandler "./logging.log" Logger.DEBUG
      let fh' = setCommonFormatter fh
      Logger.updateGlobalLogger logName (Logger.setHandlers [fh'])
      Logger.updateGlobalLogger logName (Logger.setLevel Logger.DEBUG)
      Logger.noticeM logName "Start Log"
    handler :: Error -> IO ()
    handler err = do
      print err
      print "hello"
