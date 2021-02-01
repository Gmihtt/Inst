module Types.Domain.Threads where

import qualified APP.Scripts.Sockets.API as API
import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

type Threads = Map.Map Text ByteString

data ThreadsMap
  = ThreadsMap
      { threads :: Threads,
        stream :: API.Stream
      }
