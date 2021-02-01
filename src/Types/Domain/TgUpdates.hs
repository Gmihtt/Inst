module Types.Domain.TgUpdates (ListOfUpdates) where

import Control.Concurrent.Chan (Chan)
import Types.Telegram.Types.Update (Update)

type ListOfUpdates = Chan Update
