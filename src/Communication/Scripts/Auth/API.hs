module Communication.Scripts.Auth.API where

import Common.Error (printDebug, throwSocketErr)
import qualified Communication.Scripts.MessagesHandler.API as MessagesHandlerAPI
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Types.Communication.Scripts.Auth.Request as RequestAuth
import qualified Types.Communication.Scripts.Auth.Response as ResponseAuth
import qualified Types.Communication.Scripts.Error as Error
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Socket as Socket

type AuthMessagesHandler = MessagesHandlerAPI.MessagesHandler InstAccount.InstUsername ResponseAuth.Response

authConnection :: Socket.Socket -> IO AuthMessagesHandler
authConnection socket = do
  liftIO $ MessagesHandlerAPI.runMessagesHandler socket getInstUsername getBsBody
  where
    getInstUsername bsBody =
      maybe
        (throwSocketErr $ "decode fail" <> show bsBody)
        (pure . InstAccount.InstUsername . ResponseAuth.username)
        (decode bsBody)
    getBsBody bsBody _ = maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)

sendAndReceiveMsg ::
  InstAccount.InstUsername ->
  AuthMessagesHandler ->
  RequestAuth.Request ->
  IO (Either Text ResponseAuth.Response)
sendAndReceiveMsg instId handler req = do
  printDebug req
  MessagesHandlerAPI.sendMsg (encode req) handler
  v <- newEmptyMVar
  getMsg v
  res <- takeMVar v
  MessagesHandlerAPI.deleteMsg instId handler
  printDebug res
  let bsError = (Error.parseCriticalError . Error.error_code) =<< ResponseAuth.error res
  pure $ maybe (Right res) Left bsError
  where
    getMsg v = do
      sleepSecond
      mbMsg <- MessagesHandlerAPI.findMsg instId handler
      maybe (getMsg v) (putMVar v) mbMsg
    sleepSecond = threadDelay 1000000
