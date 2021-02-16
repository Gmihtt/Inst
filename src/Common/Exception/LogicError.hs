module Common.Exception.LogicError (LogicError (..)) where

import Control.Exception (Exception)

newtype LogicError = LogicError String deriving (Show)

instance Exception LogicError
