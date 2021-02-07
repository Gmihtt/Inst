module Common.Error
  ( Error (..),
    throwConfigErr,
    throwRedisErr,
    throwMongoErr,
    throwTelegramErr,
    throwTgErr,
    throwSocketErr,
    throwThreadsError,
    printDebug,
    printError
  )
where

import Common.Exception.ConfigError as Error
import Common.Exception.MongoError as Error
import Common.Exception.RedisError as Error
import Common.Exception.SocketError as Error ( SocketError(..) )
import Common.Exception.TelegramError as Error
import Common.Exception.ThreadsError as Error
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Log.Logger ( debugM, errorM )
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

throwError :: Error -> IO a
throwError err = do
  printError err
  throwIO err

throwConfigErr :: String -> IO a
throwConfigErr err = throwError . Config $ Error.ConfigError err

throwRedisErr :: String -> IO a
throwRedisErr err = throwError . Redis $ Error.RedisError err

throwMongoErr :: String -> IO a
throwMongoErr err = throwError . Mongo $ Error.MongoError err

throwSocketErr :: String -> IO a
throwSocketErr err = throwError . Socket $ Error.SocketError err

throwThreadsError :: String -> IO a
throwThreadsError err = throwError . Threads $ Error.ThreadsError err

throwTgErr :: Text -> IO a
throwTgErr = throwTelegramErr Nothing

throwTelegramErr :: Maybe Int -> Text -> IO a
throwTelegramErr mbCode desc =
  throwIO . Telegram $
    TelegramError
      { code = fromMaybe 0 mbCode,
        description = desc
      }

printDebug :: Show a => a -> IO ()
printDebug msg =
  liftIO $ debugM "BotLogger.Main" (show msg)

printError :: Show a => a -> IO ()
printError msg =
  liftIO $ errorM "BotLogger.Main" (show msg)