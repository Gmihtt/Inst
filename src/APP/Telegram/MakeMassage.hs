module APP.Telegram.MakeMassage where

import qualified Types.Telegram.Types.Update as Update
import qualified Types.Telegram.Types.Message as Message

mkMassage :: Update.Update -> IO Message.Message
mkMassage update = 