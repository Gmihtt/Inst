module APP.Telegram.BotMain where

import APP.Telegram.GetUpdates (getUpdates)
import APP.Telegram.ParseUpdate ( parseUpdate )
import Common.Flow ( Flow ) 
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import qualified Types.Telegram.Types.Update as Update

run :: Maybe Integer -> Flow ()
run updateId = do
  updates <- getUpdates updateId
  mapM_ parseUpdate updates
  let newUpdateId = getMax $ map Update.update_id updates
  run newUpdateId
  where
    getMax :: [Integer] -> Maybe Integer
    getMax [] = updateId
    getMax list = Just $ last list + 1
    second = 1000000
