{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.GetUpdates
  ( GetUpdates,
  )
where

import Data.Text (Text)
import Servant
import Telegram.Types.Communication.Response (Response)
import Telegram.Types.Domain.Update (Updates)
import qualified Telegram.Types.Methods.GetUpdates as Method

type GetUpdates =
  Capture "token" Text
    :> "getUpdates"
    :> ReqBody '[JSON] Method.GetUpdates
    :> Get '[JSON] (Response Updates)
