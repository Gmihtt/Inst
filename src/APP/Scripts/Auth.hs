module APP.Scripts.Auth where

import qualified APP.Scripts.Scripts as Scripts
import qualified APP.Scripts.Sockets.API as API
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr)
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Types.Domain.Scripts.Auth as Auth
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Threads as Threads

authConnection :: Socket.Socket -> IO Threads.ThreadsMap
authConnection socket = do
  liftIO $ Scripts.runConnection socket getUsername
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . Auth.response_username) (decode bsBody)

auth :: Text -> Text -> Flow Auth.Response
auth username password = do
  env <- ask
  let threadsMap = Environment.authThreads env
  let req = Auth.mkRequest username password
  liftIO $ print req
  liftIO $ Scripts.sendAndReceiveMsg username threadsMap req
