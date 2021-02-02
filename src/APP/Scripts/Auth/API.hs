module APP.Scripts.Auth.API where

import qualified APP.Scripts.Socket.API as API
import qualified APP.Scripts.Socket.Connection as Connection
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr)
import Common.Flow (Flow)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Scripts.Auth as ScriptsAuth
import qualified Types.Domain.Socket as Socket

auth :: Text -> Text -> Flow ScriptsAuth.Response
auth username password = do
  env <- ask
  let threadsMap = Environment.authManager env
  let req = ScriptsAuth.mkRequest username password
  liftIO $ print req
  liftIO $ sendAndReceiveMsg username threadsMap req

authConnection :: Socket.Socket -> IO Manager.AuthManager
authConnection socket = do
  liftIO $ Connection.runConnection socket getUsername getBsBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ScriptsAuth.response_username) (decode bsBody)
    getBsBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> Manager.AuthManager -> ScriptsAuth.Request -> IO ScriptsAuth.Response
sendAndReceiveMsg key (Manager.Manager manager stream) req = do
  API.sendMsg (encode req) stream
  script <- newEmptyMVar
  getMsg script
  bsRes <- takeMVar script
  Map.delete key manager
  pure bsRes
  where
    getMsg script = do
      sleepSecond
      mbMsg <- Map.lookup key manager
      maybe (getMsg script) (putMVar script) mbMsg
    sleepSecond = threadDelay 1000000
