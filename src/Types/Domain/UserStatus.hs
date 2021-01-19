module Types.Domain.UserStatus where

data LoginStatus 
  = Free
  | WaitAuth
  deriving (Show)

mkLoginStatus :: String -> LoginStatus
mkLoginStatus "WaitAuth" = WaitAuth
mkLoginStatus _ = Free

data UserStatus 
  = Login LoginStatus
  | MainMenu
  deriving (Show)

mkUserStatus :: String -> UserStatus 
mkUserStatus "Login Free" = Login Free
mkUserStatus "Login WaitAuth" = Login WaitAuth
mkUserStatus  _ = MainMenu
