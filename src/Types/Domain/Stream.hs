module Types.Domain.Stream where

import qualified Control.Concurrent.Chan as Chan
import Data.ByteString.Lazy (ByteString)

data Stream
  = Stream
      { fromServerToScript :: Chan.Chan ByteString,
        fromScriptToServer :: Chan.Chan ByteString
      }

initStream :: IO Stream
initStream = do
  a <- Chan.newChan
  b <- Chan.newChan
  pure $
    Stream
      { fromServerToScript = a,
        fromScriptToServer = b
      }

sendMsgToScript :: ByteString -> Stream -> IO ()
sendMsgToScript msg (Stream toScript _) = Chan.writeChan toScript msg

getMsgToScript :: Stream -> IO ByteString
getMsgToScript (Stream toScript _) = Chan.readChan toScript

putMsgForServer :: ByteString -> Stream -> IO ()
putMsgForServer res (Stream _ toServer) = Chan.writeChan toServer res

getMsgForServer :: Stream -> IO ByteString
getMsgForServer (Stream _ toServer) = Chan.readChan toServer
