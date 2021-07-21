{-# LANGUAGE RecordWildCards #-}

module Types.Domain.MessagesHandler where

import qualified Control.Concurrent.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Types.Domain.Socket as Socket
import Data.Text (Text)

type Username = Text

type UsernameGetter = ByteString -> IO Username

type Messages msg = Map.Map Username msg

type MsgGetter msg = ByteString -> Maybe msg -> IO msg

data MessagesHandler msg
  = MessagesHandler
      { messages :: Messages msg,
        stream :: Socket.Stream
      }

initMessagesHandler :: Socket.Stream -> IO (MessagesHandler msg)
initMessagesHandler stream = do
  messages <- Map.empty
  let manager = MessagesHandler {..}
  pure manager