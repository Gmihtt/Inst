module APP.Telegram.BotMain where

import qualified APP.Scripts.Auth.API as ScriptsAuth
import qualified APP.Telegram.GetUpdates as GetUpdates
import qualified APP.Telegram.ParseUpdates as ParseUpdates
import qualified Common.Environment as Environment
import Common.Flow (Flow)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (getChanContents)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import qualified Types.Telegram.Types.Update as Update

run :: Maybe Integer -> Environment.Environment -> IO ()
run updateId env = do
  updates <- GetUpdates.execute updateId env
  ParseUpdates.execute updates env
