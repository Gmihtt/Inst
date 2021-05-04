module Common.Exception.ProxyError (ProxyError (..)) where

import Control.Exception (Exception)

newtype ProxyError = ProxyError String deriving (Show)

instance Exception ProxyError
