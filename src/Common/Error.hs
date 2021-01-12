module Common.Error (Error (..), throwConfigErr, throwTelegramErr) where

import Common.Exception.ConfigError as Error
import Common.Exception.TelegramError as Error
import Control.Exception (Exception, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Error = Config Error.ConfigError | Telegram Error.TelegramError deriving (Show)

instance Exception Error

throwConfigErr :: String -> IO a
throwConfigErr err = throwIO . Config $ Error.ConfigError err

throwTelegramErr :: Maybe Int -> Text -> IO a
throwTelegramErr code desc =
  throwIO . Telegram $
    TelegramError
      { code = fromMaybe 0 code,
        description = desc
      }