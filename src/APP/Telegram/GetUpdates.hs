{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.GetUpdates
  ( execute,
  )
where

import API.CallTelegram (callTelegram)
import qualified API.Routes as API
import qualified Common.Environment as Environment
import Common.Error (throwTelegramErr, throwTgErr)
import Common.Flow (Flow)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeList2Chan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Types.Domain.TgUpdates (ListOfUpdates)
import qualified Types.Telegram.Response as Response
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

getBody :: Response.Response a -> Flow a
getBody response =
  liftIO $
    if Response.ok response
      then
        maybe
          (throwTgErr "Function: getBody. When try to get 'result' of 'response'")
          pure
          (Response.result response)
      else
        throwTelegramErr
          (Response.error_code response)
          (fromMaybe "Function: getBody. When try to get 'description' of 'response'" $ Response.description response)
