{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Telegram.Response
  ( Response (..),
    Body (..),
    getBody,
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

data Body a
  = Body a
  | Error (Int, Text)
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Body a) where
  toJSON = toJson

instance (FromJSON a) => FromJSON (Body a) where
  parseJSON = parseJson

data Response a
  = Response
      { ok :: Bool,
        result :: Maybe a,
        description :: Maybe Text,
        error_code :: Maybe Int
      }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Response a) where
  toJSON = toJson

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = parseJson

getBody :: Response a -> Either (Int, Text) a
getBody response =
  if ok response
    then maybe (Left (666, "The impossible has happened")) Right $ result response
    else Left (fromMaybe 0 $ error_code response, fromMaybe "" $ description response)
