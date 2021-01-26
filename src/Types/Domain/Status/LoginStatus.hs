module Types.Domain.Status.LoginStatus where

data LoginStatus 
  = Free
  | WaitAuth
  deriving (Show)

mkLoginStatus :: String -> LoginStatus
mkLoginStatus "WaitAuth" = WaitAuth
mkLoginStatus _ = Free