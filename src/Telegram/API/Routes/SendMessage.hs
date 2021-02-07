{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.SendMessage where

import Data.Aeson (Value)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Servant
import Telegram.Types.Communication.Response (Response)
import qualified Telegram.Types.Domain.Chat as Chat
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Domain.Update (Updates)
import qualified Telegram.Types.Methods.SendMessage as SendMessage

type SendMessage =
  Capture "token" T.Text
    :> "sendMessage"
    :> ReqBody '[JSON] SendMessage.SendMessage
    :> Post '[JSON] (Response Message.Message)
