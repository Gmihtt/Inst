{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.TelegramUserStatus where

import qualified Common.Environment as Environment
import Common.Error
import Common.Flow (Flow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack)
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

data TelegramUserStatus
  = TelegramUserStatus
      { user :: User.User,
        userStatus :: Maybe TgUserStatus.TgUserStatus,
        msgByUser :: Maybe Text
      }
  deriving (Show)

updateUserStatus :: User.User -> TgUserStatus.TgUserStatus -> Flow Bool
updateUserStatus user status = do
  printUserAction user (Just status) Nothing
  env <- ask
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show (User.id user)
  liftIO $ TgUsersStatus.updateUserStatus uId status tgUsersStatus

getUserStatus :: Int -> Flow (Maybe TgUserStatus.TgUserStatus)
getUserStatus userId = do
  env <- ask
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show userId
  liftIO $ TgUsersStatus.getUserStatus uId tgUsersStatus

printUserAction :: User.User -> Maybe TgUserStatus.TgUserStatus -> Maybe Text -> Flow ()
printUserAction user userStatus msgByUser =
  liftIO . printDebug $ TelegramUserStatus {..}
