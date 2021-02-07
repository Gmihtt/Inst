module Types.Domain.Status.TgUsersStatus where

import qualified Control.Concurrent.Map as Map
import Data.Text (Text)
import Types.Domain.Status.TgUserStatus (TgUserStatus)

type TgUsersStatus = Map.Map Text TgUserStatus

empty :: IO TgUsersStatus
empty = Map.empty

updateUserStatus :: Text -> TgUserStatus -> TgUsersStatus -> IO Bool
updateUserStatus = Map.insert

getUserStatus :: Text -> TgUsersStatus -> IO (Maybe TgUserStatus)
getUserStatus = Map.lookup
