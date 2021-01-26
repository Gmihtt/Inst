{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.ParseUpdate where

import Common.Error (throwTgErr)
import Common.Flow (Flow)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.CallbackQuery as CallbackQuery
import qualified APP.SelectingLogic.CallBackCases as CallBackCases
import Types.Telegram.Types.Message (Message)
import Types.Telegram.Types.Update (Update)
import qualified Types.Telegram.Types.Update as Update
import qualified APP.SelectingLogic.UsualCases as UsualCases
import qualified APP.Telegram.Messages.FlowMessages as Messages

parseUpdate :: Update -> Flow (Response Message)
parseUpdate update =
  case mbCallBack of
    Nothing -> getMsg mbMsg >>= UsualCases.execute
    Just cb -> maybe (getMsg mbMsg >>= Messages.oldMsg) (CallBackCases.execute cb) (CallbackQuery.callback_message cb)
  where
    mbCallBack = Update.callback_query update
    mbMsg = Update.message update

getMsg :: Maybe Message -> Flow Message
getMsg =
  maybe (throwTgErr "Function: parseUpdate. In 'update' field 'message' is Nothing") pure