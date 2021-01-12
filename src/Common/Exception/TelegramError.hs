module Common.Exception.TelegramError (TelegramError (..)) where

import Control.Exception (Exception)
import Data.Text (Text)

data TelegramError
  = TelegramError
      { code :: Int,
        description :: Text
      }
  deriving (Show)

instance Exception TelegramError
