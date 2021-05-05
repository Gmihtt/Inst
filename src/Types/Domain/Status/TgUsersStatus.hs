module Types.Domain.Status.TgUsersStatus where

import qualified Control.Concurrent.Map as Map
import Control.Concurrent.MVar
import Data.Text (Text)
import Types.Domain.Status.TgUserStatus (TgUserStatus)

type TgUsersStatus = Map.Map Text (MVar TgUserStatus)

empty :: IO TgUsersStatus
empty = Map.empty

updateUserStatus :: Text -> TgUserStatus -> TgUsersStatus -> IO Bool
updateUserStatus userId status usersStatus = do
  s <- newMVar status
  Map.insert userId s usersStatus

getUserStatus :: Text -> TgUsersStatus -> IO (Maybe TgUserStatus)
getUserStatus userId usersStatus = do
  mbVal <- Map.lookup userId usersStatus
  maybe (pure Nothing) ((Just <$>) . takeMVar) mbVal
