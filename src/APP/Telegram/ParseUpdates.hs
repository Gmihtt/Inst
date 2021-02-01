{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.ParseUpdates where

import qualified APP.Scripts.Scripts as Scripts
import qualified APP.SelectingLogic.CallBackCases as CallBackCases
import qualified APP.SelectingLogic.UsualCases as UsualCases
import qualified APP.Telegram.Messages.FlowMessages as Messages
import qualified Common.Environment as Environment
import Common.Error (throwTgErr)
import Common.Flow (Flow)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Types.Domain.TgUpdates (ListOfUpdates)
import Types.Telegram.Response (Response (..))
import qualified Types.Telegram.Types.CallbackQuery as CallbackQuery
import Types.Telegram.Types.Message (Message)
import Types.Telegram.Types.Update (Update)
import qualified Types.Telegram.Types.Update as Update

execute :: ListOfUpdates -> Environment.Environment -> IO ()
execute updates env = do
  update <- readChan updates
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
