module Common.Error
  ( Error (..),
    throwConfigErr,
    throwRedisErr,
    throwMongoErr,
    throwTelegramErr,
    throwTgErr,
    throwSocketErr,
    throwThreadsError,
  )
where

import Common.Exception.ConfigError as Error
import Common.Exception.MongoError as Error
import Common.Exception.RedisError as Error
import Common.Exception.SocketError as Error
import Common.Exception.TelegramError as Error
import Common.Exception.ThreadsError as Error
import Control.Exception (Exception, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Error
  = Config Error.ConfigError
  | Telegram Error.TelegramError
  | Redis Error.RedisError
  | Mongo Error.MongoError
  | Socket Error.SocketError
  | Threads Error.ThreadsError
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

throwThreadsError :: String -> IO a
throwThreadsError err = throwIO . Threads $ Error.ThreadsError err

throwTgErr :: Text -> IO a
throwTgErr = throwTelegramErr Nothing

throwTelegramErr :: Maybe Int -> Text -> IO a
throwTelegramErr mbCode desc =
  throwIO . Telegram $
    TelegramError
      { code = fromMaybe 0 mbCode,
        description = desc
      }
