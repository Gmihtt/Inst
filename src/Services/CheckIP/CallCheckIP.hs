module Services.CheckIP.CallCheckIP where

import qualified Common.Environment as Environment
import Common.Error (throwCheckIPErr)
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Servant.Client
import qualified Services.CheckIP.Routes as API
import qualified Types.Communication.CheckIP.GeoIP as GeoIP
import qualified Types.Domain.Proxy as Proxy

callCheckIP :: ClientM a -> Flow a
callCheckIP method = do
  env <- getEnvironment
  let eManager = Environment.manager env
  res <- liftIO $ runClientM method (mkClientEnv eManager (BaseUrl Https "freegeoip.app" 443 ""))
  either (liftIO . throwCheckIPErr . show) pure res

checkIP :: Proxy.Proxy -> Flow (Proxy.Proxy, Bool)
checkIP proxy = do
  let ip = Proxy.ip proxy
  (,) proxy . GeoIP.isLondon <$> callCheckIP (API.checkIP ip)
