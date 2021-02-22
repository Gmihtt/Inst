{-# LANGUAGE OverloadedStrings #-}

module App.Bot.GetUpdates
  ( execute,
  )
where

import qualified Common.Environment as Environment
import Common.Error (printError, throwTelegramErr, throwTgErr)
import Common.Flow (Flow)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeList2Chan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Telegram.API.Methods.CallTelegram (callTelegram)
import qualified Telegram.API.Routes as API
import qualified Telegram.Types.Communication.Response as Response
import qualified Telegram.Types.Domain.Update as Update
import Types.Domain.TgUpdates (ListOfUpdates)

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

getBody :: Show a => Response.Response a -> Flow a
getBody response =
  liftIO $
    if Response.ok response
      then
        maybe
          (liftIO (printError response) >> throwTgErr "Function: getBody. When try to get 'result' of 'response'")
          pure
          (Response.result response)
      else do
        liftIO (printError response)
        throwTelegramErr
          (Response.error_code response)
          (fromMaybe "Function: getBody. When try to get 'description' of 'response'" $ Response.description response)
