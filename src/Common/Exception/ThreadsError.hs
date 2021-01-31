module Common.Exception.ThreadsError where

import Control.Exception (Exception)

newtype ThreadsError = ThreadsError String deriving (Show)

instance Exception ThreadsError
