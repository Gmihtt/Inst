{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes
  ( getUpdates,
    sendMessage,
  )
where

import API.GetUpdates (GetUpdates)
import API.SendMessage (SendMessage)
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
import qualified Types.Telegram.Methods.SendMessage as SendMessage
import Types.Telegram.Response (Response)
import Types.Telegram.Types.Message (Message)
import Types.Telegram.Types.Update (Updates)

type API = GetUpdates :<|> SendMessage

resApi :: Proxy API
resApi = Proxy

getUpdates :: Text -> Maybe Integer -> Maybe Int -> Maybe Int -> ClientM (Response Updates)

sendMessage :: Text -> SendMessage.SendMessage -> ClientM (Response Message)
getUpdates :<|> sendMessage = client resApi
