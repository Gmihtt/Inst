{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Common.Json
  ( ToJSON (..),
    FromJSON (..),
    Value (..),
    toJsonDrop,
    toJson,
    parseJsonDrop,
    parseJson,
    encodeBs,
    decodeBs,
  )
where

import Data.Aeson
  ( FromJSON,
    GFromJSON,
    GToJSON,
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON,
    Value (..),
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    parseJSON,
    toJSON,
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import GHC.Generics (Generic (Rep))

toJsonDrop :: forall a. (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = drop prefix,
        omitNothingFields = True
      }

toJson :: forall a. (Generic a, GToJSON Zero (Rep a)) => a -> Value
toJson = toJsonDrop 0

parseJsonDrop :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions {fieldLabelModifier = drop prefix}

parseJson :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseJson = parseJsonDrop 0

encodeBs :: ToJSON a => a -> B.ByteString
encodeBs = LB.toStrict . Aeson.encode

decodeBs :: FromJSON a => B.ByteString -> Maybe a
decodeBs = Aeson.decode . LB.fromStrict
