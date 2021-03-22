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
  | AddAccountCode Username Password InstId
  | AccountMenu InstId
  | WaitStart InstId
  | ChoseStatistics InstId
  | Logout InstId
  deriving (Show)

data AdminStatus
  = SelectTgUser
  | MessageFromBot
  | ShowInstAccounts
  | AccountInfo
  deriving (Show)
