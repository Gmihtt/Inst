module APP.App (app) where

import APP.Telegram.BotMain (run)
import Configs.Config (getToken)
import Control.Exception ( SomeException, Exception, catch )
import Common.Error (Error(..))
import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

app :: IO ()
app = catch (do
  manager <- newManager tlsManagerSettings
  token <- getToken
  run Nothing manager token)
  handler
  where 
    handler :: Error -> IO ()
    handler = print
