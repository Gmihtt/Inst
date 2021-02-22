{-# LANGUAGE OverloadedStrings #-}

module App.Bot.ParseUpdates where

import qualified App.Bot.Messages.FlowMessages as Messages
import qualified App.Bot.Selecting.Users.API as UserAPI
import qualified Common.Environment as Environment
import Common.Error (throwTgErr)
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import Telegram.Types.Domain.Update (Update)
import qualified Telegram.Types.Domain.Update as Update
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUpdates as TgUpdates

execute :: TgUpdates.ListOfUpdates -> Environment.Environment -> IO ()
execute updates env = do
  update <- TgUpdates.getUpdate updates
  forkIO $ runReaderT (parseUpdate update) env
  execute updates env

parseUpdate :: Update -> Flow ()
parseUpdate update = do
  checkEditMsg
  where
    checkEditMsg =
      case Update.edited_message update of
        Nothing -> checkChannelPost
        Just _ -> pure ()
    checkChannelPost =
      case Update.channel_post update of
        Nothing -> checkEditedChannelPost
        Just _ -> pure ()
    checkEditedChannelPost =
      case Update.edited_channel_post update of
        Nothing -> checkShippingQuery
        Just _ -> pure ()
    checkShippingQuery =
      case Update.shipping_query update of
        Nothing -> checkPreCheckoutQuery
        Just _ -> pure ()
    checkPreCheckoutQuery =
      case Update.pre_checkout_query update of
        Nothing -> checkPoll
        Just _ -> pure ()
    checkPoll =
      case Update.poll update of
        Nothing -> checkPollAnswer
        Just _ -> pure ()
    checkPollAnswer =
      case Update.poll_answer update of
        Nothing -> checkInlineQuery
        Just _ -> pure ()
    checkInlineQuery =
      case Update.inline_query update of
        Nothing -> checkChosenInlineResult
        Just _ -> pure ()
    checkChosenInlineResult =
      case Update.chosen_inline_result update of
        Nothing -> checkCallBack
        Just _ -> pure ()
    checkCallBack = do
      case Update.callback_query update of
        Nothing -> getMsg >>= UserAPI.messageFromUser
        Just cb -> maybe (getMsg >>= Messages.oldMsg) (checkUserStatus cb) (CallbackQuery.callback_message cb)
      pure ()
    getMsg = do
      let mbMsg = Update.message update
      liftIO $ maybe (throwTgErr "Function: parseUpdate. In 'update' field 'message' is Nothing") pure mbMsg

checkUserStatus :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
checkUserStatus cb msg = do
  mbStatus <- Common.getUserStatus userId
  case mbStatus of
    Nothing -> do
      Common.updateUserStatus userId (TgUserStatus.TgUser TgUserStatus.MainMenu)
      UserAPI.mainMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.MainMenu) -> UserAPI.mainMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.Help) -> UserAPI.helpMenu cb msg
    Just (TgUserStatus.TgUser TgUserStatus.ListOfAccounts) -> UserAPI.listOfAccounts cb msg
    Just (TgUserStatus.TgUser (TgUserStatus.AccountMenu instAcc)) -> UserAPI.accountMenu cb msg instAcc
    Just (TgUserStatus.TgUser (TgUserStatus.Logout instAcc)) -> UserAPI.logout cb msg instAcc
    Just _ -> Messages.strangeMessage msg
  where
    userId = User.id $ CallbackQuery.callback_from cb
