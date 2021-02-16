{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Selecting.Users.MessageFromUser where

import qualified App.Bot.Execution.Users.Login as Login
import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import qualified Common.FlowEnv as Common
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified MongoDB.Queries as Mongo
import qualified Redis.Queries as Redis
import Telegram.API.Methods.SendMessage (sendMessage)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

messageFromUser :: Message.Message -> Flow (Response Message.Message)
messageFromUser msg = do
  case Message.from msg of
    Nothing -> Messages.msgForEmptyUser msg
    Just user -> checkStatus msg (User.id user)

checkStatus :: Message.Message -> Int -> Flow (Response Message.Message)
checkStatus msg userId = do
  let text = T.words $ fromMaybe "" (Message.text msg)
  case text of
    ["/start"] -> setMainMenu
    ["/help"] -> setHelpMenu
    ["/delete", login] -> do
      liftIO $ print login
      let tg_id = T.pack $ show userId
      Mongo.deleteInstAccount tg_id login "accounts"
      Common.putInstAccs userId
      setMainMenu
    _ -> do
      status <- Common.getUserStatus userId
      maybe setMainMenu (choseAction msg userId) status
  where
    setHelpMenu = do
      Common.updateUserStatus
        userId
        (TgUserStatus.TgUser TgUserStatus.Help)
      Messages.helpMessage msg
    setMainMenu = do
      Common.updateUserStatus
        userId
        (TgUserStatus.TgUser TgUserStatus.MainMenu)
      Messages.mainMenu msg

choseAction :: Message.Message -> Int -> TgUserStatus.TgUserStatus -> Flow (Response Message.Message)
choseAction msg userId (TgUserStatus.TgAdmin status) =
  case status of
    TgUserStatus.SelectTgUser -> undefined
    TgUserStatus.MessageFromBot -> undefined
    TgUserStatus.ShowInstAccounts -> undefined
    TgUserStatus.AccountInfo -> undefined
choseAction msg userId (TgUserStatus.TgUser status) =
  case status of
    TgUserStatus.AddAccountLogin -> Login.login msg userId text
    TgUserStatus.AddAccountPassword username -> Login.password msg userId username text
    TgUserStatus.AddAccountCode username password instId -> Login.authCode msg userId instId username password text
    _ -> Messages.strangeMessage msg
  where
    text = fromMaybe "" $ Message.text msg
