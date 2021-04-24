module App.Scripts.Auth.API where

import qualified Common.Environment as Environment
import Common.Error (printDebug, throwSocketErr)
import Common.Flow (Flow, getEnvironment)
import qualified Communication.Sockets.API as SocketsAPI
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Types.Communication.Auth.Request as RequestAuth
import qualified Types.Communication.Auth.Response as ResponseAuth
import qualified Types.Domain.ThreadManager as Manager

authLogin :: Text -> Text -> Flow ResponseAuth.Response
authLogin username password = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestLogin username password
  liftIO $ printDebug req
  liftIO $ sendAndReceiveMsg username authManager req

doubleAuth :: Text -> Text -> Flow ResponseAuth.Response
doubleAuth username code = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestDoubleAuth username code
  liftIO $ printDebug req
  liftIO $ sendAndReceiveMsg username authManager req

susCode :: Text -> Text -> Flow ResponseAuth.Response
susCode username code = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestSus username code
  liftIO $ printDebug req
  liftIO $ sendAndReceiveMsg username authManager req

authConnection :: SocketsAPI.Socket -> IO Manager.AuthManager
authConnection socket = do
  liftIO $ SocketsAPI.runConnection socket getUsername getBsBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseAuth.username) (decode bsBody)
    getBsBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> Manager.AuthManager -> RequestAuth.Request -> IO ResponseAuth.Response
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
