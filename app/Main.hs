module Main where

import API.Routes (run)
import Common.Errors (tokenError)
import Configs.Config (getToken)
import Control.Monad.Trans.Except (runExceptT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  eToken <- runExceptT getToken
  either tokenError (run Nothing manager) eToken
