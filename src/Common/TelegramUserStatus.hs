{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.TelegramUserStatus where

import qualified Common.Environment as Environment
import Common.Error
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, pack)
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.Status.TgUsersStatus as TgUsersStatus

data TelegramUserStatus
  = TelegramUserStatus
      { user :: User.User,
        userStatus :: Maybe TgUserStatus.TgUserStatus,
        msgByUser :: Maybe Text
      }
  deriving (Show)

setMainMenu :: User.User -> Flow Bool
setMainMenu user = updateUserStatus user (TgUserStatus.TgUser TgUserStatus.MainMenu)

setHelp :: User.User -> Flow Bool
setHelp user = updateUserStatus user (TgUserStatus.TgUser TgUserStatus.Help)

setListOfAccounts :: User.User -> Flow Bool
setListOfAccounts user = updateUserStatus user (TgUserStatus.TgUser TgUserStatus.ListOfAccounts)

setAddAccountLogin :: User.User -> Flow Bool
setAddAccountLogin user = updateUserStatus user (TgUserStatus.TgUser TgUserStatus.AddAccountLogin)

setAddAccountPassword :: User.User -> InstAccount.InstUsername -> Flow Bool
setAddAccountPassword user username =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.AddAccountPassword username)

setAddDoubleAuth :: User.User -> InstAccount.InstUsername -> TgUserStatus.Password -> Flow Bool
setAddDoubleAuth user username password =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.AddDoubleAuth username password)

setAddSusCode :: User.User -> InstAccount.InstUsername -> TgUserStatus.Password -> Flow Bool
setAddSusCode user username password =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.AddSusCode username password)

setPhoneCheck :: User.User -> InstAccount.InstUsername -> TgUserStatus.Password -> Flow Bool
setPhoneCheck user username password =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.PhoneCheck username password)

setAccountMenu :: User.User -> InstAccount.InstId -> Flow Bool
setAccountMenu user instId =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.AccountMenu instId)

setWaitStart :: User.User -> InstAccount.InstId -> Flow Bool
setWaitStart user instId =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.WaitStart instId)

setChoseStatistics :: User.User -> InstAccount.InstId -> Flow Bool
setChoseStatistics user instId =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.ChoseStatistics instId)

setLogout :: User.User -> InstAccount.InstId -> Flow Bool
setLogout user instId =
  updateUserStatus user (TgUserStatus.TgUser $ TgUserStatus.Logout instId)

updateUserStatus :: User.User -> TgUserStatus.TgUserStatus -> Flow Bool
updateUserStatus user status = do
  printUserAction user (Just status) Nothing
  env <- getEnvironment
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show (User.id user)
  liftIO $ TgUsersStatus.updateUserStatus uId status tgUsersStatus

getUserStatus :: Int -> Flow (Maybe TgUserStatus.TgUserStatus)
getUserStatus userId = do
  env <- getEnvironment
  let tgUsersStatus = Environment.tgUsersStatus env
  let uId = pack $ show userId
  liftIO $ TgUsersStatus.getUserStatus uId tgUsersStatus

printUserAction :: User.User -> Maybe TgUserStatus.TgUserStatus -> Maybe Text -> Flow ()
printUserAction user userStatus msgByUser =
  liftIO . printDebug $ TelegramUserStatus {..}
