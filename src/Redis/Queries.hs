module Redis.Queries (getValue, putValue, deleteValue) where

import qualified Common.Environment as Environment
import Common.Error (throwRedisErr)
import Common.Flow (Flow, getEnvironment)
import Common.Json (FromJSON, ToJSON, decodeBs, encodeBs)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import qualified Database.Redis as Redis

getValue :: (ToJSON a, FromJSON b) => a -> Flow (Maybe b)
getValue key = do
  env <- getEnvironment
  let conn = Environment.conn env
  res <- liftIO . Redis.runRedis conn . Redis.get $ encodeBs key
  let value = fromRight Nothing res
  pure $ decodeBs =<< value

putValue :: (ToJSON a, ToJSON b) => a -> b -> Flow ()
putValue key val = do
  env <- getEnvironment
  let conn = Environment.conn env
  status <- liftIO . Redis.runRedis conn $ Redis.set (encodeBs key) (encodeBs val)
  liftIO $ either (throwRedisErr . show) pure (void status)

deleteValue :: ToJSON a => a -> Flow ()
deleteValue key = do
  env <- getEnvironment
  let conn = Environment.conn env
  liftIO . Redis.runRedis conn . Redis.del $ [encodeBs key]
  pure ()
