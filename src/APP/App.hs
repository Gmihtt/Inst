module APP.App (app) where

import APP.Telegram.BotMain (run)
import Common.Error (Error (..))
import Configs.Config (getToken)
import Control.Exception (Exception, SomeException, catch)
import Database.MongoDB ( connect, host, close )
import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

app :: IO ()
app =
  catch
    ( do
        manager <- newManager tlsManagerSettings
        token <- getToken
        pipe <- connect (host "127.0.0.1")
        run Nothing manager token
        close pipe
    )
    handler
  where
    handler :: Error -> IO ()
    handler = print
