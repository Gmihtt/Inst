{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.ParseUpdate where

import APP.Telegram.Buttons.BaseMenu (baseKeyboard)
import qualified APP.Telegram.Login.Login as Login
import qualified APP.Telegram.Messages.Messages as Messages
import APP.Telegram.SendMessage (sendMessage)
import Common.Error (throwTgErr)
import Common.Flow (Flow)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.CallbackQuery as CallbackQuery
import Types.Telegram.Types.Keyboard.InlineKeyboardMarkup (InlineKeyboardMarkup)
import Types.Telegram.Types.Message (Message)
import Types.Telegram.Types.Update (Update (..))
import qualified Types.Telegram.Types.User as User

parseUpdate :: Update -> Flow (Response Message)
parseUpdate update =
  case mbCallBack of
    Nothing -> getMsg mbMsg >>= baseCase
    Just cb -> maybe (getMsg mbMsg >>= oldCase) (callBackCase cb) (CallbackQuery.callback_message cb)
  where
    mbCallBack = callback_query update
    mbMsg = message update

baseCase :: Message -> Flow (Response Message)
baseCase msg = sendMessage (Just baseKeyboard) (Messages.baseMenu msg)

oldCase :: Message -> Flow (Response Message)
oldCase msg = do
  sendMessage Nothing (Messages.oldMsg msg)
  sendMessage (Just baseKeyboard) (Messages.baseMenu msg)

callBackCase :: CallbackQuery.CallbackQuery -> Message -> Flow (Response Message)
callBackCase callBack msg =
  case CallbackQuery.callback_data callBack of
    "login" -> Login.execute msg userId >>= sendMessage Nothing
    "run" -> undefined
    "stop" -> undefined
    "payment" -> undefined
    "statistics" -> undefined
  where
    userId = User.id $ CallbackQuery.callback_from callBack

getMsg :: Maybe Message -> Flow Message
getMsg =
  maybe (throwTgErr "Function: parseUpdate. In 'update' field 'message' is Nothing") pure
