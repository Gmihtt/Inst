{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.SendMessage where

import Data.Aeson (Value)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Servant
import qualified Types.Telegram.Methods.SendMessage as SendMessage
import Types.Telegram.Response (Response)
import qualified Types.Telegram.Types.Chat as Chat
import qualified Types.Telegram.Types.Message as Message
import Types.Telegram.Types.Update (Updates)

type SendMessage =
  Capture "token" T.Text
    :> "sendMessage"
    :> ReqBody '[JSON] SendMessage.SendMessage
    :> Post '[JSON] (Response Message.Message)
