{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes
  ( getUpdates,
    sendMessage,
    sendDocument,
    sendPhoto,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Servant
import Servant.Client (ClientM, client)
import Telegram.API.Routes.GetUpdates (GetUpdates)
import Telegram.API.Routes.SendDocument (SendDocument)
import Telegram.API.Routes.SendMessage (SendMessage)
import Telegram.API.Routes.SendPhoto (SendPhoto)
import Telegram.Types.Communication.Response (Response)
import Telegram.Types.Domain.Message (Message)
import Telegram.Types.Domain.Update (Updates)
import qualified Telegram.Types.Methods.GetUpdates as GetUpdates
import qualified Telegram.Types.Methods.SendDocument as SendDocument
import qualified Telegram.Types.Methods.SendMessage as SendMessage
import qualified Telegram.Types.Methods.SendPhoto as SendPhoto

type API = GetUpdates :<|> SendMessage :<|> SendDocument :<|> SendPhoto

resApi :: Proxy API
resApi = Proxy

getUpdates :: Text -> GetUpdates.GetUpdates -> ClientM (Response Updates)

sendMessage :: Text -> SendMessage.SendMessage -> ClientM (Response Message)

sendDocument :: Text -> Text -> (ByteString, SendDocument.File) -> ClientM (Response Message)

sendPhoto :: Text -> Text -> (ByteString, SendPhoto.Photo) -> ClientM (Response Message)
getUpdates :<|> sendMessage :<|> sendDocument :<|> sendPhoto = client resApi
