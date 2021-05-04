{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.SendPhoto
  ( SendPhoto,
  )
where

import Data.Text (Text)
import Servant
import Servant.Multipart
import Telegram.Types.Communication.Response (Response)
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Domain.Update (Updates)
import qualified Telegram.Types.Methods.SendPhoto as Method

type SendPhoto =
  Capture "token" Text
    :> "sendPhoto"
    :> Capture "chat_id" Text
    :> MultipartForm Tmp Method.Photo
    :> Post '[JSON] (Response Message.Message)
