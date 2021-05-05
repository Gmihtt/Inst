{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.ListOfAccounts where

import qualified App.Bot.Execution.Users.ShowAccounts as ShowAccounts
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.ProxyStatus as ProxyStatus

listOfAccounts :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
listOfAccounts callBack msg =
  case CallbackQuery.callback_data callBack of
    "Add" -> do
      tryGetProxy user msg
    "Back" -> ShowAccounts.back msg user
    username -> do
      let uId = T.pack $ show userId
      mbInstAcc <- Mongo.findInstAccountByLogin uId username
      maybe (Messages.strangeMessage msg) (ShowAccounts.selectedAcc msg user) mbInstAcc
  where
    user = CallbackQuery.callback_from callBack
    userId = User.id user

tryGetProxy :: User.User -> Message -> Flow (Response Message)
tryGetProxy user msg = do
  env <- getEnvironment
  let proxyManager = Environment.proxyManager env
  Messages.waitMessage msg 
  eProxyLoad <- liftIO $ ProxyStatus.getProxyLoad proxyManager
  case eProxyLoad of
    Left time -> Messages.timeBlockMessage time msg
    Right (proxyLoad, countTry) -> do
      if countTry >= 10
        then do
          liftIO $ ProxyStatus.addProxyLoad proxyLoad 0 proxyManager
          Messages.timeBlockMessage 5 msg
        else
          ShowAccounts.addAccount proxyLoad countTry msg user