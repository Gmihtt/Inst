module Services.Proxy.CallProxy where

import qualified Common.Environment as Environment
import Common.Error (throwProxyErr)
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Servant.Client

callProxy :: ClientM a -> Flow a
callProxy method = do
  env <- getEnvironment
  let eManager = Environment.manager env
  res <- liftIO $ runClientM method (mkClientEnv eManager (BaseUrl Https "panel.proxyline.net" 443 ""))
  either (liftIO . throwProxyErr . show) pure res
