{-# LANGUAGE DeriveGeneric #-}

module Types.Communication.Proxy.ListOfProxy where

import Common.Json
  ( FromJSON (..),
    parseJson,
  )
import GHC.Generics (Generic)
import Types.Domain.Proxy (Proxy)
import Prelude hiding (id)

data ListOfProxy
  = ListOfProxy
      { count :: Int,
        results :: [Proxy]
      }
  deriving (Show, Eq, Generic)

instance FromJSON ListOfProxy where
  parseJSON = parseJson
