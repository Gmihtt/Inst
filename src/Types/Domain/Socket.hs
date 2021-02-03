module Types.Domain.Socket where

data Socket
  = Socket
      { host :: String,
        port :: Int,
        path :: String
      }
  deriving (Show)

mkSocket :: String -> Int -> String -> Socket
mkSocket s_host s_port s_path =
  Socket
    { host = s_host,
      port = s_port,
      path = s_path
    }
