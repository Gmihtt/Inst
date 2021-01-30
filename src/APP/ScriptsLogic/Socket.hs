module APP.ScriptsLogic.Socket (app) where

import           Control.Concurrent  (forkIO, ThreadId)
import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import Common.Error (throwSocketErr)
import qualified Network.WebSockets as WS
import Data.Aeson (decode, encode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified APP.ScriptsLogic.Stream as Stream
import qualified Types.Domain.Scripts.Auth as Auth

app :: (ToJSON a, FromJSON b) => Stream.Stream a b -> WS.ClientApp ()
app stream conn = do
  _ <- getMsg stream conn
  pure ()

getMsg :: FromJSON b => Stream.Stream a b -> WS.Connection -> IO ThreadId 
getMsg stream conn = forkIO . forever $ do
  bsMsg <- WS.receiveData conn
  msg <- maybe (throwSocketErr "decode error") pure (decode bsMsg)
  Stream.putMsgForServer msg stream

putMsg :: ToJSON a => Stream.Stream a b -> WS.Connection -> IO ThreadId 
putMsg stream conn = forkIO . forever $ do
  msg <- Stream.getMsgToScript stream
  WS.sendBinaryData conn (encode msg)