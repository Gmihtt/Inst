module Types.Domain.Status.TgUserStatus where

import Data.Text (Text)
import Types.Domain.ProxyStatus ( ProxyParams )

data TgUserStatus = TgUser UserStatus | TgAdmin AdminStatus deriving (Show)

type Username = Text

type Password = Text

type InstId = Text

data UserStatus
  = MainMenu
  | Help
  | ListOfAccounts
  | AddAccountLogin ProxyParams
  | AddAccountPassword ProxyParams Username
  | AddDoubleAuth ProxyParams Username Password
  | AddSusCode ProxyParams Username Password
  | PhoneCheck ProxyParams Username Password
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
