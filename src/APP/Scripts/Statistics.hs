module APP.Scripts.Statistics where

import qualified APP.Scripts.Scripts as Scripts
import qualified APP.Scripts.Sockets.API as API
import qualified Common.Environment as Environment
import Common.Error (throwSocketErr)
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Types.Domain.Scripts.Statistics as Statistics
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Threads as Threads

statConnection :: Socket.Socket -> IO Threads.ThreadsMap
statConnection socket = do
  liftIO $ Scripts.runConnection socket getUsername
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . Statistics.response_inst_id) (decode bsBody)
