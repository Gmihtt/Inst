module Common.Error
  ( Error (..),
    throwConfigErr,
    throwRedisErr,
    throwMongoErr,
    throwTelegramErr,
    throwTgErr,
    throwSocketErr,
  )
where

import Common.Exception.ConfigError as Error
import Common.Exception.TelegramError as Error
import Common.Exception.RedisError as Error
import Common.Exception.MongoError as Error
import Common.Exception.SocketError as Error
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Error 
  = Config Error.ConfigError 
  | Telegram Error.TelegramError 
  | Redis Error.RedisError
  | Mongo Error.MongoError
  | Socket Error.SocketError
  deriving (Show)

instance Exception Error

throwConfigErr :: String -> IO a
throwConfigErr err = throwIO . Config $ Error.ConfigError err

throwRedisErr :: String -> IO a
throwRedisErr err = throwIO . Redis $ Error.RedisError err

throwMongoErr :: String -> IO a
throwMongoErr err = throwIO . Mongo $ Error.MongoError err

throwSocketErr :: String -> IO a
throwSocketErr err = throwIO . Socket $ Error.SocketError err

throwTgErr :: Text -> IO a
throwTgErr = throwTelegramErr Nothing

throwTelegramErr :: Maybe Int -> Text -> IO a
throwTelegramErr code desc =
  throwIO . Telegram $
    TelegramError
      { code = fromMaybe 0 code,
        description = desc
      }
