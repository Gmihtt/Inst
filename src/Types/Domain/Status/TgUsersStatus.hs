module Types.Domain.Status.TgUsersStatus where

import Types.Domain.Status.TgUserStatus ( TgUserStatus )
import Data.Text (Text)
import qualified Control.Concurrent.Map as Map

type TgUsersStatus = Map.Map Text TgUserStatus

empty :: IO TgUsersStatus
empty = Map.empty

updateUserStatus :: Text -> TgUserStatus -> TgUsersStatus -> IO Bool
updateUserStatus = Map.insert

getUserStatus :: Text -> TgUsersStatus -> IO (Maybe TgUserStatus)
getUserStatus = Map.lookup