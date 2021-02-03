{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes
  ( getUpdates,
    sendMessage,
  )
where

import Telegram.API.Routes.GetUpdates (GetUpdates)
import Telegram.API.Routes.SendMessage (SendMessage)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import Data.Text (Text)
import Servant
import Servant.Client (ClientM, client)
import qualified Telegram.Types.Methods.SendMessage as SendMessage
import Telegram.Types.Communication.Response (Response)
import Telegram.Types.Domain.Message (Message)
import Telegram.Types.Domain.Update (Updates)

type API = GetUpdates :<|> SendMessage

resApi :: Proxy API
resApi = Proxy

getUpdates :: Text -> Maybe Integer -> Maybe Int -> Maybe Int -> ClientM (Response Updates)

sendMessage :: Text -> SendMessage.SendMessage -> ClientM (Response Message)
getUpdates :<|> sendMessage = client resApi
