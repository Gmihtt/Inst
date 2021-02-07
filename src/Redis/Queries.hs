module Redis.Queries (getValue, putValue) where

import qualified Common.Environment as Environment
import Common.Error (throwRedisErr)
import Common.Flow (Flow)
import Common.Json (FromJSON, ToJSON, encodeBs, decodeBs)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Either (fromRight)
import qualified Database.Redis as Redis

getValue :: (ToJSON a, FromJSON b) => a -> Flow (Maybe b)
getValue key = do
  env <- ask
  let conn = Environment.conn env
  res <- liftIO . Redis.runRedis conn . Redis.get $ encodeBs key
  let value = fromRight Nothing res
  pure $ decodeBs =<< value

putValue :: (ToJSON a, ToJSON b) => a -> b -> Flow ()
putValue key val = do
  env <- ask
  let conn = Environment.conn env
  status <- liftIO . Redis.runRedis conn $ Redis.set (encodeBs key) (encodeBs val)
  liftIO $ either (throwRedisErr . show) pure (void status)
