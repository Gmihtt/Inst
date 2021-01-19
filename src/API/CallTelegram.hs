module API.CallTelegram (callTelegram) where

import Common.Error (throwTelegramErr)
import Control.Monad.Trans.Reader (ask)
import Common.Flow (Flow)
import qualified Common.Flow as Environment
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (Manager)
import Data.Text (pack)
import Servant.Client

callTelegram :: ClientM a -> Flow a
callTelegram method = do
  env <- ask
  let eManager = Environment.manager env
  res <- liftIO $ runClientM method (mkClientEnv eManager (BaseUrl Https "api.telegram.org" 443 ""))
  either (throwTelegramErr Nothing . pack . show) pure res