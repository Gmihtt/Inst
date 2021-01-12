{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.GetUpdates
  ( getUpdates,
  )
where

import qualified API.Routes as API
import Common.Error (throwTelegramErr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.Update as Update

getUpdates :: Maybe Integer -> Manager -> Text -> IO Update.Updates
getUpdates updateId manager token = do
  eUpdates <- API.callTelegram (API.getUpdates token updateId Nothing (Just 1)) manager
  getBody eUpdates

getBody :: Response a -> IO a
getBody response =
  if ok response
    then maybe (throwTelegramErr (Just 666) "The impossible has happened") pure $ result response
    else throwTelegramErr (error_code response) (fromMaybe "" $ description response)
