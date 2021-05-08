{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Execution.Users.MainMenu where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import Prelude hiding (id)

accounts :: Message.Message -> User.User -> Flow (Response Message.Message)
accounts msg user = do
  Common.setListOfAccounts user
  instAccs <- Common.getInstAccs (User.id user)
  Message.showInstAccs msg (map InstAccount.login instAccs)

help :: Message.Message -> User.User -> Flow (Response Message.Message)
help msg user = do
  Common.setHelp user
  Message.helpMessage msg
