{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import qualified API.GetUpdates as ApiGetUpdates
import qualified API.SendMessage as ApiSendMessage
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client hiding (Response)
import qualified Types.Telegram.Methods.SendMessage as SendMessage
import Types.Telegram.Response (Response, getBody)
import qualified Types.Telegram.Types.Message as Message
import qualified Types.Telegram.Types.Update as Update

type API = ApiGetUpdates.GetUpdates :<|> ApiSendMessage.SendMessage

resApi :: Proxy API
resApi = Proxy

getUpdates :: T.Text -> Maybe Integer -> Maybe Int -> Maybe Int -> ClientM (Response Update.Updates)

sendMessage :: T.Text -> SendMessage.SendMessage -> ClientM (Response Message.Message)
getUpdates :<|> sendMessage = client resApi

callTelegram :: ClientM a -> Manager -> IO (Either ClientError a)
callTelegram method manager = runClientM method (mkClientEnv manager (BaseUrl Https "api.telegram.org" 443 ""))

run :: Maybe Integer -> Manager -> T.Text -> IO ()
run updateId manager token = do
  eResponse <- callTelegram (getUpdates token updateId Nothing (Just 1)) manager
  case eResponse of
    Left err -> print err
    Right response ->
      case getBody response of
        Left err -> print err
        Right updates -> do
          let messages = mapMaybe Update.message updates
          let sendMessages = map SendMessage.mkSendMessage messages
          mapM_ callSendMessages sendMessages
          let newUpdateId = getMax $ map Update.update_id updates
          run newUpdateId manager token
  where
    --threadDelay (6 * second)

    callSendMessages :: SendMessage.SendMessage -> IO ()
    callSendMessages sM = do
      res <- callTelegram (sendMessage token sM) manager
      print res
    getMax [] = updateId
    getMax list = Just $ last list + 1
    second = 1000000
--https://api.telegram.org/bot1436919530:AAF-dK8XXr4id5i4N_k_83_AY2pKDA9c5rE/getUpdates
