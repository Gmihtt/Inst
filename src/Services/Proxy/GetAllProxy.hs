module Services.Proxy.GetAllProxy where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Services.Proxy.CallProxy (callProxy)
import qualified Services.Proxy.Routes as API
import Types.Communication.Proxy.ListOfProxy (ListOfProxy)

getAllProxy :: Flow ListOfProxy
getAllProxy = do
  env <- getEnvironment
  let apiKey = Environment.proxyApiKey env
  callProxy $ API.getAllProxy (Just apiKey)
