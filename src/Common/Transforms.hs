module Common.Transforms where
  
import Data.ByteString (ByteString, pack, unpack)
import Codec.Binary.UTF8.String ( decode, encode )

unpackBs :: ByteString -> String
unpackBs = decode . unpack
packBs :: Show a => a -> ByteString
packBs = pack . encode . show