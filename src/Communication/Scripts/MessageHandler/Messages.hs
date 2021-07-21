module Communication.Scripts.MessageHandler.Messages where

import qualified Types.Domain.MessagesHandler as MessagesHandler
import qualified Control.Concurrent.Map as Map
import Data.Text (Text)

addMsg :: Text -> msg -> MessagesHandler.MessagesHandler msg -> IO Bool
addMsg username msg handler = Map.insert username msg (MessagesHandler.messages handler)

findMsg :: Text -> MessagesHandler.MessagesHandler msg -> IO (Maybe msg)
findMsg username handler = Map.lookup username (MessagesHandler.messages handler)

deleteMsg :: Text -> MessagesHandler.MessagesHandler msg -> IO Bool
deleteMsg username handler = Map.delete username (MessagesHandler.messages handler)