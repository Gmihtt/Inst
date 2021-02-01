module Types.Domain.UserStatus where

import Types.Domain.Status.LoginStatus

data UserStatus
  = Login LoginStatus
  | MainMenu
  deriving (Show)

setWaitAuth :: UserStatus
setWaitAuth = Login WaitAuth

setFree :: UserStatus
setFree = Login Free

mkUserStatus :: String -> UserStatus
mkUserStatus "Login Free" = Login Free
mkUserStatus "Login WaitAuth" = Login WaitAuth
mkUserStatus _ = MainMenu
