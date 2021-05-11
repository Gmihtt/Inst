{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.MessageFromUser where

import qualified App.Bot.Execution.Admin.AdminMenu as Admin
import qualified App.Bot.Execution.Admin.ShowUser as Admin
import qualified App.Bot.Execution.Users.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as StatAPI
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Communication.Scripts.Info.Request as InfoRequest
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

messageFromUser :: Message.Message -> Flow (Response Message.Message)
messageFromUser msg = do
  case Message.from msg of
    Nothing -> Messages.msgForEmptyUser msg
    Just user -> checkStatus msg user

checkStatus :: Message.Message -> User.User -> Flow (Response Message.Message)
checkStatus msg user = do
  let text = fromMaybe "" (Message.text msg)
  Common.printUserAction user Nothing (Just text)
  case text of
    "/start" -> setMainMenu
    "/help" -> setHelpMenu
    "/reboot" -> reboot
    "/admin konechno proxy" -> Admin.proxyLoad msg
    "/admin konechno active" -> Admin.showAllUsers msg
    _ -> do
      status <- Common.getUserStatus userId
      maybe setMainMenu (choseAction msg user) status
  where
    setHelpMenu = do
      Common.setHelp user
      Messages.helpMessage msg
    setMainMenu = do
      Common.setMainMenu user
      Messages.mainMenu msg
    reboot = do
      env <- getEnvironment
      let statManager = Environment.statisticsManager env
      let uId = T.pack $ show userId
      instAccs <- Mongo.findInstAccsByTgId uId
      liftIO $ mapM (StatAPI.sendMsg statManager . RequestStat.mkLogoutReq . InstAccount.id) instAccs
      Common.dropInstAccs userId
      Mongo.deleteTgUser uId
      setMainMenu
    userId = User.id user

choseAction :: Message.Message -> User.User -> TgUserStatus.TgUserStatus -> Flow (Response Message.Message)
choseAction _ _ (TgUserStatus.TgAdmin status) = undefined
choseAction msg user (TgUserStatus.TgUser status) =
  case status of
    TgUserStatus.AddAccountLogin proxy -> Login.login proxy msg user text
    TgUserStatus.AddAccountPassword proxy username -> do
      if T.length text > 5
        then Login.password proxy msg user username text
        else do
          Common.setListOfAccounts user
          Login.addProxyTry proxy
          Messages.failAuthMsg msg
    TgUserStatus.AddDoubleAuth proxy username accCode -> do
      let code = T.pack $ filter C.isDigit (T.unpack accCode)
      if T.length code /= 6 
        then do
          Common.setListOfAccounts user
          Login.addProxyTry proxy
          Messages.failAuthMsg msg
        else Login.doubleAuth proxy msg user username accCode text
    TgUserStatus.AddSusCode proxy username accCode -> do
      let code = T.pack $ filter C.isDigit (T.unpack accCode)
      if T.length code /= 6 
        then do
          Common.setListOfAccounts user
          Login.addProxyTry proxy
          Messages.failAuthMsg msg
        else Login.sus proxy msg user username code text
    TgUserStatus.PhoneCheck proxy username password -> Login.phoneCheck proxy msg user username password text
    _ -> do
      Common.setMainMenu user
      Messages.strangeMessage msg
  where
    text = fromMaybe "" $ Message.text msg
