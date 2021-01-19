{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.GetUpdates
  ( getUpdates,
  )
where

import API.CallTelegram (callTelegram)
import Control.Monad.Trans.Reader (ask)
import qualified API.Routes as API
import Common.Error (throwTelegramErr, throwTgErr)
import Common.Flow (Flow)
import qualified Common.Flow as Environment
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.Update as Update

getUpdates :: Maybe Integer -> Flow Update.Updates
getUpdates updateId = do
  env <- ask
  let eToken = Environment.token env
  eUpdates <- callTelegram (API.getUpdates eToken updateId Nothing (Just 1))
  getBody eUpdates

getBody :: Response a -> Flow a
getBody response =
  if ok response
    then maybe (throwTgErr "Function: getBody. When try to get 'result' of 'response'") pure $ result response
    else
      throwTelegramErr
        (error_code response)
        (fromMaybe "Function: getBody. When try to get 'description' of 'response'" $ description response)
