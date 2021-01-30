module APP.App (app) where

import APP.Telegram.BotMain (run)
import Common.Error (Error (..))
import Common.Environment (mkEnv)
import qualified Configs.Config as Config
import Control.Exception (Exception, SomeException, catch)
import qualified Database.MongoDB as MongoDB
import qualified Database.Redis as Redis
import qualified System.Process as System
import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

app :: IO ()
app =
  catch
    ( do
        manager <- newManager tlsManagerSettings
        token <- Config.getToken
        pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
        pLogin <- Config.getLoginPath
        processLogin <- System.createProcess_ "login script" (System.proc "node" [pLogin])
        pStat <- Config.getStatPath
        processStat <- System.createProcess_ "statistics script" (System.proc "node" [pStat])
        conn <- Redis.checkedConnect Redis.defaultConnectInfo
        mongoDB <- Config.getDataBase
        let env = mkEnv manager token pipe conn mongoDB
        run Nothing env
        MongoDB.close pipe
    )
    handler
  where
    handler :: Error -> IO ()
    handler err = do
      print err
      print "hello"
