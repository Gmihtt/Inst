{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.SendMessage where

import qualified Data.Text as T
import Servant
import Telegram.Types.Communication.Response (Response)
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Methods.SendMessage as SendMessage

type SendMessage =
  Capture "token" T.Text
    :> "sendMessage"
    :> ReqBody '[JSON] SendMessage.SendMessage
    :> Post '[JSON] (Response Message.Message)
