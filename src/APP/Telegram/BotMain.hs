module APP.Telegram.BotMain where

import qualified APP.Telegram.GetUpdates as GetUpdates
import qualified APP.Telegram.ParseUpdates as ParseUpdates
import Common.Flow ( Flow ) 
import qualified Common.Environment as Environment
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Concurrent.Chan (getChanContents)
import Network.HTTP.Client (Manager)
import qualified Types.Telegram.Types.Update as Update

run :: Maybe Integer -> Environment.Environment -> IO ()
run updateId env = do
  updates <- GetUpdates.execute updateId env
  ParseUpdates.execute updates env