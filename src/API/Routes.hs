{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import Control.Concurrent
import Data.Aeson (Value)
import Servant
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS 
import Servant.Client

type GetUpdates = Capture "token" T.Text  :> "getUpdates" :> QueryParam "offset" Int :> Get '[JSON] Value

type API = 
      GetUpdates


resApi :: Proxy API
resApi = Proxy

getUpdates :: T.Text -> Maybe Int -> ClientM Value
getUpdates = client resApi

run :: T.Text -> IO ()
run token = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (getUpdates token Nothing) (mkClientEnv manager' (BaseUrl Https "api.telegram.org" 443 ""))
  print res
  --threadDelay (6 * second)
  where
    second = 1000000
--https://api.telegram.org/bot1436919530:AAF-dK8XXr4id5i4N_k_83_AY2pKDA9c5rE/getUpdates