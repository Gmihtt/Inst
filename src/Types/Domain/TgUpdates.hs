module Types.Domain.TgUpdates where

import Common.Flow (Flow)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class (liftIO)
import Telegram.Types.Domain.Update (Update)

type ListOfUpdates = Chan.Chan Update

empty :: Flow ListOfUpdates
empty = liftIO Chan.newChan

addUpdates :: ListOfUpdates -> [Update] -> Flow ()
addUpdates listOfUpdates updates = liftIO $ Chan.writeList2Chan listOfUpdates updates

getUpdate :: ListOfUpdates -> Flow Update
getUpdate = liftIO . Chan.readChan
