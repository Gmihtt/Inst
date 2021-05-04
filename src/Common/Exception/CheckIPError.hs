module Common.Exception.CheckIPError (CheckIPError (..)) where

import Control.Exception (Exception)

newtype CheckIPError = CheckIPError String deriving (Show)

instance Exception CheckIPError
