{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Admin.ShowUser where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Info.Request as InfoRequest
import qualified Types.Communication.Scripts.Info.Response as InfoResponse
import qualified App.Scripts.Info.API as InfoAPI

showAllUsers :: Message.Message -> Flow (Response Message.Message)
showAllUsers msg = do
  env <- getEnvironment
  let infoManager = Environment.infoManager env
  res <- liftIO $ InfoAPI.sendAndReceiveMsg "" infoManager $ InfoRequest.mkAllStatusReq ""
  let (users, count) = (fromMaybe [] $ InfoResponse.users_info res, fromMaybe 0 $ InfoResponse.user_count_active res)
  Messages.smthMessage (users, count) msg
