{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.MessageFromUser where

import qualified App.Bot.Execution.Users.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Info.API as InfoAPI
import qualified App.Scripts.Statistics.API as StatAPI
import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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
    "/admin konechno" -> admin
    _ -> do
      status <- Common.getUserStatus userId
      maybe setMainMenu (choseAction msg user) status
  where
    setHelpMenu = do
      Common.updateUserStatus
        user
        (TgUserStatus.TgUser TgUserStatus.Help)
      Messages.helpMessage msg
    setMainMenu = do
      Common.updateUserStatus
        user
        (TgUserStatus.TgUser TgUserStatus.MainMenu)
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
    admin = do
      env <- getEnvironment
      let infoManager = Environment.infoManager env
      res <- liftIO $ InfoAPI.sendAndReceiveMsg "1" infoManager $ InfoRequest.mkAllStatusReq "1"
      Messages.smthMessage res msg
    userId = User.id user

choseAction :: Message.Message -> User.User -> TgUserStatus.TgUserStatus -> Flow (Response Message.Message)
choseAction _ _ (TgUserStatus.TgAdmin status) =
  case status of
    TgUserStatus.SelectTgUser -> undefined
choseAction msg user (TgUserStatus.TgUser status) =
  case status of
    TgUserStatus.AddAccountLogin proxyLoad countTry -> Login.login proxyLoad countTry msg user text
    TgUserStatus.AddAccountPassword proxyLoad countTry username -> do
      if T.length text > 5
        then Login.password proxyLoad countTry msg user username text
        else do
          Common.updateUserStatus
            user
            (TgUserStatus.TgUser TgUserStatus.MainMenu)
          Messages.failAuthMsg msg
    TgUserStatus.AddDoubleAuth proxyLoad countTry username password -> Login.doubleAuth proxyLoad countTry msg user username password text
    TgUserStatus.AddSusCode proxyLoad countTry username password -> Login.sus proxyLoad countTry msg user username password text
    TgUserStatus.PhoneCheck proxyLoad countTry username password -> Login.phoneCheck proxyLoad countTry msg user username password text
    _ -> Messages.strangeMessage msg
  where
    text = fromMaybe "" $ Message.text msg
