module Types.Domain.Status.TgUserStatus where

import Data.Text (Text)
import Types.Domain.ProxyLoad

data TgUserStatus = TgUser UserStatus | TgAdmin AdminStatus deriving (Show)

type Username = Text

type Password = Text

type InstId = Text

type CountTry = Int

data UserStatus
  = MainMenu
  | Help
  | ListOfAccounts
  | AddAccountLogin ProxyLoad CountTry
  | AddAccountPassword ProxyLoad CountTry Username
  | AddDoubleAuth ProxyLoad CountTry Username Password
  | AddSusCode ProxyLoad CountTry Username Password
  | PhoneCheck ProxyLoad CountTry Username Password
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
