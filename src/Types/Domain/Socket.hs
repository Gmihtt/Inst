module Types.Domain.Socket where

data Socket = Socket {
  host :: String,
  port :: Int,
  path :: String
} deriving (Show)

mkSocket :: String -> Int -> String -> Socket
mkSocket host port path = 
  Socket {
    host = host,
    port = port,
    path = path
  }