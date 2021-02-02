module Types.Domain.Status.ScriptStatus where

data ScriptStatus
  = ShowAcc
  | WaitAccount
  | ShowAccMenu
  | ConfirmStop
  deriving (Show)

mkLoginStatus :: String -> ScriptStatus
mkLoginStatus "WaitAccount" = WaitAccount
mkLoginStatus "ShowAccMenu" = ShowAccMenu
mkLoginStatus "ConfirmStop" = ConfirmStop
mkLoginStatus _ = ShowAcc
