module APP.ScriptsLogic.Auth where

import qualified Network.WebSockets as WS
import qualified APP.ScriptsLogic.Stream as Stream

app :: Stream.Stream a b -> WS.ClientApp ()
app stream conn = do
  stream

--execute :: String	-> Int -> String 
--execute host port path = do

