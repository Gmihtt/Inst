module Types.Domain.Status.TgUserStatus where

import Data.Text (Text)
import Types.Domain.InstAccount (InstId, InstUsername)

data TgUserStatus = TgUser UserStatus | TgAdmin AdminStatus deriving (Show)

type Password = Text

data UserStatus
  = MainMenu
  | Help
  | ListOfAccounts
  | AddAccountLogin
  | AddAccountPassword InstUsername
  | AddDoubleAuth InstUsername Password
  | AddSusCode InstUsername Password
  | PhoneCheck InstUsername Password
  | AccountMenu InstId
  | WaitStart InstId
  | ChoseStatistics InstId
  | Logout InstId
  deriving (Show)

data AdminStatus
  = AdminMenu
  | SelectTgUser
  | WaitTgUsername
  | WaitInstUsername
  | ShowUser
  | SelectAdmin
  | WaitAdminUsername
  | ManageAdmin
  deriving (Show)
