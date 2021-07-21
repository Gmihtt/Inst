{-# LANGUAGE RecordWildCards #-}

module Types.Domain.MessagesHandler where

import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Types.Domain.InstAccount (InstUsername)
import qualified Types.Domain.Socket as Socket

type Messages key msg = Map.Map key msg

type KeyGetter key = ByteString -> IO key

type MsgGetter msg = ByteString -> Maybe msg -> IO msg

data MessagesHandler key msg
  = MessagesHandler
      { messages :: Messages key msg,
        stream :: Socket.Stream
      }

initMessagesHandler :: Socket.Stream -> IO (MessagesHandler key msg)
initMessagesHandler stream = do
  messages <- Map.empty
  pure MessagesHandler {..}
