module Redis.Queries (getValue, putValue) where

import Common.Flow (Flow)
import Control.Monad (void)
import qualified Common.Environment as Environment
import Control.Monad.Trans.Reader (ask)
import qualified Database.Redis as Redis
import qualified Common.Transforms as Common
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.ByteString (ByteString)
import Common.Error (throwRedisErr)

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