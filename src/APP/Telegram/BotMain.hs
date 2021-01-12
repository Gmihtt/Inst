module APP.Telegram.BotMain where

import APP.Telegram.GetUpdates (getUpdates)
import APP.Telegram.SendMessage (sendMessage)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import qualified Types.Telegram.Types.Update as Update

run :: Maybe Integer -> Manager -> Text -> IO ()
run updateId manager token = do
  updates <- getUpdates updateId manager token
  let messages = mapMaybe Update.message updates
  mapM_ (sendMessage manager token) messages
  let newUpdateId = getMax $ map Update.update_id updates
  run newUpdateId manager token
  where
    getMax :: [Integer] -> Maybe Integer
    getMax [] = updateId
    getMax list = Just $ last list + 1
    second = 1000000
