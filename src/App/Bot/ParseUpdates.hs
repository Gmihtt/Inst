{-# LANGUAGE OverloadedStrings #-}

module App.Bot.ParseUpdates where

import qualified App.SelectingLogic.CallBackCases as CallBackCases
import qualified App.SelectingLogic.UsualCases as UsualCases
import qualified App.Bot.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Error (throwTgErr)
import Common.Flow (Flow)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Types.Domain.TgUpdates as TgUpdates
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.CallbackQuery as CallbackQuery
import Telegram.Types.Domain.Message (Message)
import Telegram.Types.Domain.Update (Update)
import qualified Telegram.Types.Domain.Update as Update

execute :: TgUpdates.ListOfUpdates -> Environment.Environment -> IO ()
execute updates env = do
  update <- TgUpdates.getUpdate updates
  forkIO $ runReaderT (parseUpdate update) env
  execute updates env

parseUpdate :: Update -> Flow ()
parseUpdate update = do
  case mbCallBack of
    Nothing -> getMsg mbMsg >>= UsualCases.execute
    Just cb -> maybe (getMsg mbMsg >>= Messages.oldMsg) (CallBackCases.execute cb) (CallbackQuery.callback_message cb)
  pure ()
  where
    mbCallBack = Update.callback_query update
    mbMsg = Update.message update

getMsg :: Maybe Message -> Flow Message
getMsg mbMsg = liftIO $ maybe (throwTgErr "Function: parseUpdate. In 'update' field 'message' is Nothing") pure mbMsg
