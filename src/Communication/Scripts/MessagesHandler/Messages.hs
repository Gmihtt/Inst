module Communication.Scripts.MessagesHandler.Messages where

import qualified Control.Concurrent.Map as Map
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Types.Domain.MessagesHandler as MessagesHandler

addMsg :: (Eq key, Hashable key) => key -> msg -> MessagesHandler.MessagesHandler key msg -> IO Bool
addMsg key msg handler = Map.insert key msg (MessagesHandler.messages handler)

findMsg :: (Eq key, Hashable key) => key -> MessagesHandler.MessagesHandler key msg -> IO (Maybe msg)
findMsg key handler = Map.lookup key (MessagesHandler.messages handler)

deleteMsg :: (Eq key, Hashable key) => key -> MessagesHandler.MessagesHandler key msg -> IO Bool
deleteMsg key handler = Map.delete key (MessagesHandler.messages handler)
