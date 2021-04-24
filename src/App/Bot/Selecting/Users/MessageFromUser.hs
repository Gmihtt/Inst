{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.MessageFromUser where

import qualified App.Bot.Execution.Users.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Scripts.Statistics.API as API
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
import qualified Types.Communication.Statistics.Request as RequestStat
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
      liftIO $ mapM (API.sendMsg statManager . RequestStat.mkLogoutReq . InstAccount.id) instAccs
      Common.dropInstAccs userId
      Mongo.deleteTgUser uId
      setMainMenu
    userId = User.id user

choseAction :: Message.Message -> User.User -> TgUserStatus.TgUserStatus -> Flow (Response Message.Message)
choseAction _ _ (TgUserStatus.TgAdmin status) =
  case status of
    TgUserStatus.SelectTgUser -> undefined
choseAction msg user (TgUserStatus.TgUser status) =
  case status of
    TgUserStatus.AddAccountLogin -> Login.login msg user text
    TgUserStatus.AddAccountPassword username -> do
      if T.length text > 5
        then Login.password msg user username text
        else do
          Common.updateUserStatus
            user
            (TgUserStatus.TgUser TgUserStatus.MainMenu)
          Messages.failAuthMsg msg
    TgUserStatus.AddDoubleAuth username password -> Login.doubleAuth msg user username password text
    TgUserStatus.AddSusCode username password -> Login.sus msg user username password text
    _ -> Messages.strangeMessage msg
  where
    text = fromMaybe "" $ Message.text msg
