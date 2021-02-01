module Common.Transforms where

import Codec.Binary.UTF8.String (decode, encode)
import Data.ByteString (ByteString, pack, unpack)

unpackBs :: ByteString -> String
unpackBs = decode . unpack

packBs :: Show a => a -> ByteString
packBs = pack . encode . show
