module Types.Domain.Stream where

import qualified Control.Concurrent.Chan as Chan
import Data.ByteString.Lazy (ByteString)

data Stream = Stream {
    fromServerToScript :: Chan.Chan ByteString,
    fromScriptToServer :: Chan.Chan ByteString
}