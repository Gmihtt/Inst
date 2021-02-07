module Telegram.API.Methods.CallTelegram (callTelegram) where

import qualified Common.Environment as Environment
import Common.Error (throwTelegramErr)
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
  liftIO $ either (throwTelegramErr Nothing . pack . show) pure res
