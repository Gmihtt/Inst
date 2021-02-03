{-# LANGUAGE OverloadedStrings #-}

module Common.Config
  ( getToken,
    getDataBase,
    getAuthSocket,
    getStatSocket,
  )
where

import Common.Error (throwConfigErr)
import Control.Monad.Cont (liftIO)
import Data.Text (Text, unpack)
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import Types.Domain.Socket (Socket, mkSocket)

getValue :: Text -> IO Text
getValue field = do
  settings <- liftIO $ Yaml.decodeFileEither "configs/config.yaml"
  body <- either (throwConfigErr . Yaml.prettyPrintParseException) pure settings
  let eValue = Yaml.parseEither (.: field) body
  either throwConfigErr pure eValue

getToken :: IO Text
getToken = do
  value <- getValue "token"
  pure $ "bot" <> value

getDataBase :: IO Text
getDataBase = getValue "mongoDB"

getAuthSocket :: IO Socket
getAuthSocket = do
  port <- unpack <$> getValue "auth_socket_port"
  host <- unpack <$> getValue "auth_socket_host"
  pure $ mkSocket host (read port) ""

getStatSocket :: IO Socket
getStatSocket = do
  port <- unpack <$> getValue "stat_socket_port"
  host <- unpack <$> getValue "stat_socket_host"
  pure $ mkSocket host (read port) ""
