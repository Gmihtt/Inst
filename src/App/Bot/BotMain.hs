module App.Bot.BotMain where

import qualified App.Scripts.Auth.API as ScriptsAuth
import qualified App.Bot.GetUpdates as GetUpdates
import qualified App.Bot.ParseUpdates as ParseUpdates
import qualified Common.Environment as Environment
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import qualified Telegram.Types.Domain.Update as Update

run :: Maybe Integer -> Environment.Environment -> IO ()
run updateId env = do
  updates <- GetUpdates.execute updateId env
  ParseUpdates.execute updates env
