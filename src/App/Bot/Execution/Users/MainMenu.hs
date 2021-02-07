{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.MainMenu where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified MongoDB.Queries as Mongo
import qualified Data.Text as T
import qualified Types.Domain.InstAccount as InstAccount
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Redis.Queries as Redis
import qualified Common.FlowEnv as Common
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import Prelude hiding (id)

accounts :: Message.Message -> Int -> Flow (Response Message.Message)
accounts msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.ListOfAccounts
  Common.updateUserStatus userId status
  instAccs <- Common.getInstAccs userId
  Message.showInstAccs msg (map InstAccount.login instAccs)

help :: Message.Message -> Int -> Flow (Response Message.Message)
help msg userId = do
  let status = TgUserStatus.TgUser TgUserStatus.Help
  Common.updateUserStatus userId status
  Message.helpMessage msg
