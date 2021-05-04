{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.SendDocument
  ( SendDocument,
  )
where

import Data.Text (Text)
import Servant
import Servant.Multipart
import Telegram.Types.Communication.Response (Response)
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Domain.Update (Updates)
import qualified Telegram.Types.Methods.SendDocument as Method

type SendDocument =
  Capture "token" Text
    :> "sendDocument"
    :> Capture "chat_id" Text
    :> MultipartForm Tmp Method.File
    :> Post '[JSON] (Response Message.Message)
