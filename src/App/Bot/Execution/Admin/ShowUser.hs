{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Admin.ShowUser where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Communication.Scripts.Info.API as InfoAPI
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified MongoDB.Queries.Usernames as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Communication.Scripts.Info.Request as InfoRequest
import qualified Types.Communication.Scripts.Info.Response as InfoResponse
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Usernames as Usernames

showAllUsers :: Message.Message -> Flow (Response Message.Message)
showAllUsers msg = do
  env <- getEnvironment
  let handler = Environment.infoMessagesHandler env
  allUsernamse <- Mongo.getAllUsernames
  res <- liftIO $ InfoAPI.sendAndReceiveMsg "" handler $ InfoRequest.mkAllStatusReq ""
  let (userInfos, count) = (fromMaybe [] $ InfoResponse.users_info res, fromMaybe 0 $ InfoResponse.user_count_active res)
  let usernames = getInstUsernames userInfos allUsernamse
  Messages.smthMessage (usernames, count) msg
  where
    getInstUsernames userInfos allUsernames =
      let ids = InstAccount.InstId . InfoResponse.id <$> userInfos
       in let checkId usernames = Usernames.instId usernames `elem` ids
           in Usernames.instUsername <$> filter checkId allUsernames
