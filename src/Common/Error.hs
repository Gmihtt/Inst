module Common.Error
  ( Error (..),
    throwConfigErr,
    throwRedisErr,
    throwMongoErr,
    throwTelegramErr,
    throwTgErr,
  )
where

import Common.Exception.ConfigError as Error
import Common.Exception.TelegramError as Error
import Common.Exception.RedisError as Error
import Common.Exception.MongoError as Error
import Control.Monad.IO.Class (liftIO)
import Common.Flow (Flow)
import Control.Exception (Exception, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Error 
  = Config Error.ConfigError 
  | Telegram Error.TelegramError 
  | Redis Error.RedisError
  | Mongo Error.MongoError
  deriving (Show)

instance Exception Error

throwConfigErr :: String -> IO a
throwConfigErr err = throwIO . Config $ Error.ConfigError err

throwRedisErr :: String -> Flow a
throwRedisErr err = liftIO . throwIO . Redis $ Error.RedisError err

throwMongoErr :: String -> Flow a
throwMongoErr err = liftIO . throwIO . Mongo $ Error.MongoError err

throwTgErr :: Text -> Flow a
throwTgErr = throwTelegramErr Nothing

throwTelegramErr :: Maybe Int -> Text -> Flow a
throwTelegramErr code desc =
  liftIO . throwIO . Telegram $
    TelegramError
      { code = fromMaybe 0 code,
        description = desc
      }
