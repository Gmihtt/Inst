{-# LANGUAGE RecordWildCards #-}

module Communication.Sockets.Socket where

data Socket
  = Socket
      { host :: String,
        port :: Int,
        path :: String
      }
  deriving (Show)

mkSocket :: String -> Int -> String -> Socket
mkSocket host port path =
  Socket {..}
