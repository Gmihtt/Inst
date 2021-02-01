module Redis.Queries (getValue, putValue) where

import qualified Common.Environment as Environment
import Common.Error (throwRedisErr)
import Common.Flow (Flow)
import qualified Common.Transforms as Common
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import qualified Database.Redis as Redis

getValue :: Show a => a -> Flow (Maybe String)
getValue key = do
  env <- ask
  let conn = Environment.conn env
  res <- liftIO . Redis.runRedis conn . Redis.get $ Common.packBs key
  let value = Common.unpackBs <$> fromRight Nothing res
  pure value

putValue :: (Show a, Show b) => a -> b -> Flow ()
putValue key val = do
  env <- ask
  let conn = Environment.conn env
  status <- liftIO . Redis.runRedis conn $ Redis.set (Common.packBs key) (Common.packBs val)
  liftIO $ either (throwRedisErr . show) pure (void status)
