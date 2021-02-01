module Common.Exception.SocketError (SocketError (..)) where

import Control.Exception (Exception)

newtype SocketError = SocketError String deriving (Show)

instance Exception SocketError
