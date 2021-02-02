module APP.Scripts.Statistics.API where

import qualified APP.Scripts.Socket.Connection as Connection
import qualified APP.Scripts.Socket.API as API
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr)
import Common.Flow (Flow)
import Control.Monad ( unless )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Maybe ( fromMaybe ) 
import qualified Types.Domain.Scripts.Statistics as ScriptsStat
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Manager as Manager
import Data.ByteString.Lazy (ByteString)

statConnection :: Socket.Socket -> IO Manager.StatisticsManager 
statConnection socket = do
  liftIO $ Connection.runConnection socket getUsername mkStatistics
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ScriptsStat.response_inst_id) (decode bsBody)

mkStatistics :: ByteString -> Maybe Statistic.Statistic -> IO Statistic.Statistic
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  users <- getUsers value
  pure $ maybe (addUsers users Statistic.empty) (addUsers users) mbStat
  where
    addUsers users stat = foldr Statistic.addUser stat users
    getUsers value =
      if ScriptsStat.response_status value
        then print (ScriptsStat.response_errorMessage value) >> pure []
        else pure . fromMaybe [] $ ScriptsStat.response_users value

sendMsg :: Text -> Manager.AuthManager -> ScriptsStat.Request -> IO ()
sendMsg key (Manager.Manager manager stream) req = do
  API.sendMsg (encode req) stream