{-# LANGUAGE OverloadedStrings #-}

module Config (getToken) where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import Data.Text ( Text)

getToken :: IO (Either String Text)
getToken = do
  settings <- Yaml.decodeFileEither "configs/config.yaml"
  pure $ either (Left . Yaml.prettyPrintParseException) parseToken settings
  where 
    parseToken = Yaml.parseEither (.: "token")
