{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.API.Routes.GetUpdates
  ( GetUpdates,
  )
where

import qualified Data.Text as T
import Servant
import Telegram.Types.Communication.Response (Response)
import Telegram.Types.Domain.Update (Updates)

type GetUpdates =
  Capture "token" T.Text
    :> "getUpdates"
    :> QueryParam "offset" Integer
    :> QueryParam "limit" Int
    :> QueryParam "timeout" Int
    :> Get '[JSON] (Response Updates)
