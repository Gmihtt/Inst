module Telegram.API.Methods.CallTelegram (callTelegram) where

import qualified Common.Environment as Environment
import Common.Error (printError)
import Common.Flow (Flow, getEnvironment)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Servant.Client

callTelegram :: ClientM a -> Flow a
callTelegram method = do
  env <- getEnvironment
  let manager = Environment.telegramManager env
  res <- liftIO $ runClientM method (mkClientEnv manager (BaseUrl Https "api.telegram.org" 443 ""))
  either (tryCallAgain method) pure res

tryCallAgain :: ClientM a -> ClientError -> Flow a
tryCallAgain method err = do
  liftIO sleepMinute
  liftIO $ printError err
  callTelegram method
  where
    sleepMinute = threadDelay $ 60 * 1000000
