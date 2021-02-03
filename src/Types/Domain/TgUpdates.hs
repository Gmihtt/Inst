module Types.Domain.TgUpdates where

import qualified Control.Concurrent.Chan as Chan
import Telegram.Types.Domain.Update (Update)

type ListOfUpdates = Chan.Chan Update

empty :: IO ListOfUpdates
empty = Chan.newChan

addUpdates :: ListOfUpdates -> [Update] -> IO ()
addUpdates = Chan.writeList2Chan

getUpdate :: ListOfUpdates -> IO Update
getUpdate = Chan.readChan