module Common.Exception.MongoError (MongoError (..)) where

import Control.Exception (Exception)

newtype MongoError = MongoError String deriving (Show)

instance Exception MongoError
