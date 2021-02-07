module Types.Domain.Status.TgUserStatus where

import Data.Text (Text)

data TgUserStatus = TgUser UserStatus | TgAdmin AdminStatus deriving (Show)

type AccLogin = Text
type InstId = Text

data UserStatus
  = MainMenu
  | Help
  | ListOfAccounts
  | AddAccountLogin
  | AddAccountPassword AccLogin
  | AccountMenu InstId
  deriving (Show)

data AdminStatus
  = SelectTgUser
  | MessageFromBot
  | ShowInstAccounts
  | AccountInfo
  deriving (Show)