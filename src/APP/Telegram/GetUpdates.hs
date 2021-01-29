{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.GetUpdates
  ( execute,
  )
where

import API.CallTelegram (callTelegram)
import qualified API.Routes as API
import Common.Error (throwTelegramErr, throwTgErr)
import Common.Flow (Flow)
import Control.Monad.IO.Class (liftIO)
import qualified Common.Environment as Environment
import Control.Monad.Trans.Reader (runReaderT, ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan ( newChan, writeList2Chan, Chan )
import Types.Domain.TgUpdates (ListOfUpdates)
import Network.HTTP.Client (Manager)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.Update as Update

execute :: Maybe Integer -> Environment.Environment -> IO ListOfUpdates
execute updateId env = do
  listOfUpdates <- newChan 
  forkIO $ runReaderT (getUpdates updateId listOfUpdates) env
  pure listOfUpdates

getUpdates :: Maybe Integer -> Chan Update.Update -> Flow ()
getUpdates updateId listOfUpdates = do
  env <- ask
  let token = Environment.token env
  eUpdates <- callTelegram (API.getUpdates token updateId Nothing (Just 1))
  updates <- getBody eUpdates
  let newUpdateId = getMax $ map Update.update_id updates
  liftIO $ writeList2Chan listOfUpdates updates
  getUpdates newUpdateId listOfUpdates  
  where
    getMax :: [Integer] -> Maybe Integer
    getMax [] = updateId
    getMax list = Just $ last list + 1
    second = 1000000


getBody :: Response a -> Flow a
getBody response =
  if ok response
    then maybe (throwTgErr "Function: getBody. When try to get 'result' of 'response'") pure $ result response
    else
      throwTelegramErr
        (error_code response)
        (fromMaybe "Function: getBody. When try to get 'description' of 'response'" $ description response)
