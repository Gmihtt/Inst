module APP.Telegram.BotMain where

import Data.Text (Text)
import Data.Maybe (mapMaybe)
import qualified Types.Telegram.Types.Update as Update
import APP.Telegram.SendMessage (sendMessage)
import APP.Telegram.GetUpdates (getUpdates)
import Network.HTTP.Client (Manager)


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