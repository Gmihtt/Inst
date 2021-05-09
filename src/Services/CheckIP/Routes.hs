{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Services.CheckIP.Routes where

import Data.Text (Text)
import Servant
import Servant.Client (ClientM, client)
import Types.Communication.CheckIP.GeoIP (GeoIP)

type CheckIP =
  "json"
    :> Capture "ip" Text
    :> Get '[JSON] GeoIP

type API = CheckIP

resApi :: Proxy API
resApi = Proxy

checkIP :: Text -> ClientM GeoIP
checkIP = client resApi
