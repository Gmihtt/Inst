module Types.Domain.Status.TgUserStatus where

import Data.Text (Text)

data TgUserStatus = TgUser UserStatus | TgAdmin AdminStatus deriving (Show)

type Username = Text

type Password = Text

type InstId = Text

data UserStatus
  = MainMenu
  | Help
  | ListOfAccounts
  | AddAccountLogin
  | AddAccountPassword Username
  | AddDoubleAuth Username Password
  | AddSusCode Username Password
  | PhoneCheck Username Password
  | AccountMenu InstId
  | WaitStart InstId
  | ChoseStatistics InstId
  | Logout Username
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
