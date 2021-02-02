module Types.Domain.Status.ScriptStatus where

data ScriptStatus
  = Run
  | Stop
  deriving (Show)

mkLoginStatus :: String -> ScriptStatus
mkLoginStatus "Run" = Run
mkLoginStatus _ = Stop
