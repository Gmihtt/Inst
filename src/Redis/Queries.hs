module Redis.Queries (getValue, putValue) where

import Common.Flow (Flow)
import Control.Monad (void)
import qualified Common.Flow as Environment
import Control.Monad.Trans.Reader (ask)
import qualified Database.Redis as Redis
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.ByteString (ByteString)
import Common.Error (throwRedisErr)

getValue :: ByteString -> Flow (Maybe ByteString)
getValue key = do
  env <- ask
  let conn = Environment.conn env
  res <- liftIO $ Redis.runRedis conn $ Redis.get key
  let value = fromRight Nothing res
  pure value

putValue :: ByteString -> ByteString -> Flow () 
putValue key val = do
  env <- ask
  let conn = Environment.conn env
  status <- liftIO $ Redis.runRedis conn $ Redis.set key val
  either (throwRedisErr . show) pure (void status)