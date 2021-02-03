module App.App (app) where

import qualified App.Scripts.Auth.API as ScriptsAuth
import qualified App.Scripts.Statistics.API as ScriptsStatistics
import App.Bot.BotMain (run)
import Common.Environment (mkEnv)
import Common.Error (Error (..))
import qualified Common.Config as Config
import Control.Concurrent (forkIO)
import Control.Exception (Exception, SomeException, catch)
import Control.Monad.Trans.Except (runExceptT)
import qualified Database.MongoDB as MongoDB
import qualified Database.Redis as Redis
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified System.Process as System

app :: IO ()
app =
  catch
    ( do
        manager <- newManager tlsManagerSettings
        token <- Config.getToken
        pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
        conn <- Redis.checkedConnect Redis.defaultConnectInfo
        mongoDB <- Config.getDataBase
        authSocket <- Config.getAuthSocket
        statSocket <- Config.getStatSocket
        authManager <- ScriptsAuth.authConnection authSocket
        statManager <- ScriptsStatistics.statConnection statSocket
        let env = mkEnv manager token pipe conn mongoDB authManager statManager
        run Nothing env
        MongoDB.close pipe
    )
    handler
  where
    handler :: Error -> IO ()
    handler err = do
      print err
      print "hello"
