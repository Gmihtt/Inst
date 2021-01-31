module APP.Scripts.Statistics where

import Data.Text (Text)
import Common.Flow (Flow)
import Data.Aeson (decode)
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import qualified Types.Domain.Scripts.Statistics as Statistics
import qualified APP.Scripts.Sockets.API as API
import qualified APP.Scripts.Scripts as Scripts
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Threads as Threads

statConnection :: Socket.Socket -> IO Threads.ThreadsMap
statConnection socket = do
  liftIO $ Scripts.runConnection socket getUsername
  where
    getUsername bsBody = 
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . Statistics.response_inst_id) (decode bsBody)