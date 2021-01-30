module APP.ScriptsLogic.Stream where

import qualified Network.WebSockets as WS
import qualified Control.Concurrent.Chan as Chan

data Stream a b = Stream {
    fromServerToScript :: Chan.Chan a,
    fromScriptToServer :: Chan.Chan b
}

initStream :: IO (Stream a b)
initStream = do
    fromServerToScript <- Chan.newChan
    fromScriptToServer <- Chan.newChan
    pure $ Stream {
        fromServerToScript = fromServerToScript,
        fromScriptToServer = fromScriptToServer
    }

sendMsgToScript :: a -> Stream a b -> IO ()
sendMsgToScript msg (Stream toScript _) = Chan.writeChan toScript msg

getMsgToScript :: Stream a b -> IO a
getMsgToScript (Stream toScript _) = Chan.readChan toScript

putMsgForServer :: b -> Stream a b -> IO ()
putMsgForServer res (Stream _ toServer) = Chan.writeChan toServer res

getMsgFromServer :: Stream a b -> IO b
getMsgFromServer (Stream _ toServer) = Chan.readChan toServer
