{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.GetUpdates
  ( GetUpdates,
  )
where

import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Servant
import Types.Telegram.Response (Response)
import Types.Telegram.Types.Update (Updates)

type GetUpdates =
  Capture "token" T.Text
    :> "getUpdates"
    :> QueryParam "offset" Integer
    :> QueryParam "limit" Int
    :> QueryParam "timeout" Int
    :> Get '[JSON] (Response Updates)
