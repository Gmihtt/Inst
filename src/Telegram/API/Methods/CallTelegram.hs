module Telegram.API.Methods.CallTelegram (callTelegram) where

import qualified Common.Environment as Environment
import Common.Error (printError)
import Common.Flow (Flow)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Servant.Client

callTelegram :: ClientM a -> Flow a
callTelegram method = do
  env <- ask
  let eManager = Environment.manager env
  res <- liftIO $ runClientM method (mkClientEnv eManager (BaseUrl Https "api.telegram.org" 443 ""))
  --liftIO $ either (throwTelegramErr Nothing . pack . show) pure res
  either (tryCallAgain method) pure res

tryCallAgain :: ClientM a -> ClientError -> Flow a
tryCallAgain method err = do
  liftIO sleepMinute
  liftIO $ printError err
  callTelegram method
  where
    sleepMinute = threadDelay $ 60 * 1000000
