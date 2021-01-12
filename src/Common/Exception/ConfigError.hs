module Common.Exception.ConfigError (ConfigError (..)) where

import Control.Exception (Exception)

newtype ConfigError = ConfigError String deriving (Show)

instance Exception ConfigError
