module Telegram.API.Methods.CallTelegram (callTelegram) where

import qualified Common.Environment as Environment
import Common.Error (printError)
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (pack)
import Servant.Client

callTelegram :: ClientM a -> Flow a
callTelegram method = do
  env <- ask
  let eManager = Environment.manager env
  res <- liftIO $ runClientM method (mkClientEnv eManager (BaseUrl Https "api.telegram.org" 443 ""))
  --liftIO $ either (throwTelegramErr Nothing . pack . show) pure res
  either (const (callTelegram method)) pure res

tryCallAgain :: ClientError -> ClientM a -> Flow a
tryCallAgain err method = do
  liftIO $ printError err
  callTelegram method
