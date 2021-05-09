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
import qualified Types.Communication.Error as Error
import qualified Types.Communication.Scripts.Auth.Request as RequestAuth
import qualified Types.Communication.Scripts.Auth.Response as ResponseAuth
import Types.Domain.Proxy (Proxy)
import qualified Types.Domain.ThreadManager as Manager

authLogin :: Text -> Text -> Proxy -> Flow (Either Text ResponseAuth.Response)
authLogin username password proxy = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestLogin username password proxy
  liftIO $ sendAndReceiveMsg username authManager req

doubleAuth :: Text -> Text -> Flow (Either Text ResponseAuth.Response)
doubleAuth username code = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestDoubleAuth username code
  liftIO $ sendAndReceiveMsg username authManager req

susCode :: Text -> Text -> Flow (Either Text ResponseAuth.Response)
susCode username code = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestSus username code
  liftIO $ sendAndReceiveMsg username authManager req

phoneCheck :: Text -> Text -> Flow (Either Text ResponseAuth.Response)
phoneCheck username code = do
  env <- getEnvironment
  let authManager = Environment.authManager env
  let req = RequestAuth.mkRequestPhoneCheck username code
  liftIO $ sendAndReceiveMsg username authManager req

authConnection :: SocketsAPI.Socket -> IO Manager.AuthManager
authConnection socket = do
  liftIO $ SocketsAPI.runConnection socket getUsername getBsBody
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseAuth.username) (decode bsBody)
    getBsBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg :: Text -> Manager.AuthManager -> RequestAuth.Request -> IO (Either Text ResponseAuth.Response)
sendAndReceiveMsg key manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
  script <- newEmptyMVar
  getMsg script
  res <- takeMVar script
  Manager.deleteTask key manager
  printDebug res
  let bsError = (Error.parseCriticalError . Error.error_code) =<< ResponseAuth.error res
  pure $ maybe (Right res) Left bsError
  where
    getMsg script = do
      sleepSecond
      mbMsg <- Manager.findTask key manager
      maybe (getMsg script) (putMVar script) mbMsg
    sleepSecond = threadDelay 1000000
