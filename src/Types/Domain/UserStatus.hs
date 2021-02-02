module Types.Domain.UserStatus where

import Types.Domain.Status.LoginStatus ( LoginStatus(..) )
import Types.Domain.Status.ScriptStatus (ScriptStatus(..))

data UserStatus
  = Login LoginStatus
  | Script ScriptStatus
  | MainMenu
  deriving (Show)

setWaitAuth :: UserStatus
setWaitAuth = Login WaitAuth

setFree :: UserStatus
setFree = Login Free

mkUserStatus :: String -> UserStatus
mkUserStatus "Login Free" = Login Free
mkUserStatus "Login WaitAuth" = Login WaitAuth
mkUserStatus "Script ShowAcc" = Script ShowAcc
mkUserStatus "Script WaitAccount" = Script WaitAccount
mkUserStatus "Script ConfirmStop" = Script ConfirmStop
mkUserStatus "Script ShowAccMenu" = Script ShowAccMenu
mkUserStatus _ = MainMenu
