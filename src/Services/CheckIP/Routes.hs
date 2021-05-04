{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Services.CheckIP.Routes where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Servant
import Servant.Client (ClientM, client)
import Servant.Multipart
import Types.Communication.CheckIP.GeoIP

type CheckIP =
  "json"
    :> Capture "ip" Text
    :> Get '[JSON] GeoIP

type API = CheckIP

resApi :: Proxy API
resApi = Proxy

checkIP :: Text -> ClientM GeoIP
checkIP = client resApi
