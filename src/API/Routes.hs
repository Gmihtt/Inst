{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import qualified API.GetUpdates as ApiGetUpdates
import qualified API.SendMessage as ApiSendMessage
import Common.Error (throwTelegramErr)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client hiding (Response)
import qualified Types.Telegram.Methods.SendMessage as SendMessage
import Types.Telegram.Response (Response)
import qualified Types.Telegram.Types.Message as Message
import qualified Types.Telegram.Types.Update as Update

type API = ApiGetUpdates.GetUpdates :<|> ApiSendMessage.SendMessage

resApi :: Proxy API
resApi = Proxy

getUpdates :: Text -> Maybe Integer -> Maybe Int -> Maybe Int -> ClientM (Response Update.Updates)

sendMessage :: Text -> SendMessage.SendMessage -> ClientM (Response Message.Message)
getUpdates :<|> sendMessage = client resApi

callTelegram :: ClientM a -> Manager -> IO a
callTelegram method manager = do
  res <- runClientM method (mkClientEnv manager (BaseUrl Https "api.telegram.org" 443 ""))
  either (throwTelegramErr Nothing . pack . show) pure res
