module APP.Scripts.Sockets.Stream where

import qualified Control.Concurrent.Chan as Chan
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import qualified Types.Domain.Stream as Stream

initStream :: IO Stream.Stream
initStream = do
  fromServerToScript <- Chan.newChan
  fromScriptToServer <- Chan.newChan
  pure $
    Stream.Stream
      { Stream.fromServerToScript = fromServerToScript,
        Stream.fromScriptToServer = fromScriptToServer
      }

sendMsgToScript :: ByteString -> Stream.Stream -> IO ()
sendMsgToScript msg (Stream.Stream toScript _) = Chan.writeChan toScript msg

getMsgToScript :: Stream.Stream -> IO ByteString
getMsgToScript (Stream.Stream toScript _) = Chan.readChan toScript

putMsgForServer :: ByteString -> Stream.Stream -> IO ()
putMsgForServer res (Stream.Stream _ toServer) = Chan.writeChan toServer res

getMsgForServer :: Stream.Stream -> IO ByteString
getMsgForServer (Stream.Stream _ toServer) = Chan.readChan toServer
