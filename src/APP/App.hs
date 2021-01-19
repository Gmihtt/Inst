module APP.App (app) where

import APP.Telegram.BotMain (run)
import Common.Error (Error (..))
import Common.Flow (mkEnv)
import Control.Monad.Trans.Reader (runReaderT)
import Configs.Config (getToken, getDataBase)
import Control.Exception (Exception, SomeException, catch)
import qualified Database.MongoDB as MongoDB
import qualified Database.Redis as Redis
import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

app :: IO ()
app =
  catch
    ( do
        manager <- newManager tlsManagerSettings
        token <- getToken
        pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
        conn <- Redis.checkedConnect Redis.defaultConnectInfo
        mongoDB <- getDataBase
        let env = mkEnv manager token pipe conn mongoDB
        runReaderT (run Nothing) env
        MongoDB.close pipe
    )
    handler
  where
    handler :: Error -> IO ()
    handler err = do
      print err
      print "hello"
