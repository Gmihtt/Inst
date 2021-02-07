module App.Scripts.Auth.API where

import qualified App.Scripts.Socket.API as API
import qualified App.Scripts.Socket.Connection as Connection
import qualified Common.Environment as Environment
import Common.Error (printDebug, throwSocketErr)
import Common.Flow (Flow)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Types.Communication.Scripts.Auth as ScriptsAuth
import qualified Types.Domain.Manager as Manager
import qualified Types.Domain.Socket as Socket

auth :: Text -> Text -> Flow ScriptsAuth.Response
auth username password = do
  env <- ask
  let authManager = Environment.authManager env
  let req = ScriptsAuth.mkRequest username password
  liftIO $ printDebug req
  liftIO $ sendAndReceiveMsg username authManager req

authConnection :: Socket.Socket -> IO Manager.AuthManager
authConnection socket = do
  liftIO $ Connection.runConnection socket getUsername getBsBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ScriptsAuth.response_username) (decode bsBody)
    getBsBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> Manager.AuthManager -> ScriptsAuth.Request -> IO ScriptsAuth.Response
sendAndReceiveMsg key manager req = do
  Manager.sendMsg (encode req) manager
  script <- newEmptyMVar
  getMsg script
  bsRes <- takeMVar script
  Manager.deleteTask key manager
  pure bsRes
  where
    getMsg script = do
      sleepSecond
      mbMsg <- Manager.findTask key manager
      maybe (getMsg script) (putMVar script) mbMsg
    sleepSecond = threadDelay 1000000
