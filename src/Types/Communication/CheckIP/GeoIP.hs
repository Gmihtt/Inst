{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Communication.CheckIP.GeoIP where

import Common.Json
  ( FromJSON (..),
    parseJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data GeoIP
  = GeoIP
      { ip :: Text,
        country_code :: Text,
        country_name :: Text,
        city :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON GeoIP where
  parseJSON = parseJson

isLondon :: GeoIP -> Bool
isLondon GeoIP {..} = country_code == "GB" && (city == "Лондон" || city == "London")
