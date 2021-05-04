{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Services.Proxy.Routes where

import Data.Text (Text)
import Servant
import Servant.Client (ClientM, client)
import Types.Communication.Proxy.ListOfProxy (ListOfProxy)

type GetAllProxy =
  "proxies"
    :> QueryParam "api_key" Text
    :> Get '[JSON] ListOfProxy

type API = "api" :> GetAllProxy

resApi :: Proxy API
resApi = Proxy

getAllProxy :: Maybe Text -> ClientM ListOfProxy
getAllProxy = client resApi
