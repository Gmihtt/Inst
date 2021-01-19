module Common.Exception.RedisError (RedisError (..)) where

import Control.Exception (Exception)

newtype RedisError = RedisError String deriving (Show)

instance Exception RedisError
