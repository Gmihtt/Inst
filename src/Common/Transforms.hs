module Common.Transforms where

import Data.Bson
import Data.Text (Text, pack, unpack)

fromIdToObjectId :: Text -> ObjectId
fromIdToObjectId = read . unpack

fromObjectIdToId :: ObjectId -> Text
fromObjectIdToId = pack . show
