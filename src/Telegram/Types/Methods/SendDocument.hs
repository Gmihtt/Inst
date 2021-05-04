{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.SendDocument where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Servant.Multipart
import qualified Telegram.Types.Domain.Chat as Chat
import qualified Telegram.Types.Domain.Message as Message

data File = File {path :: FilePath, fileType :: Text}

instance ToMultipart Tmp File where
  toMultipart file =
    MultipartData
      []
      [ FileData
          ""
          (pack $ path file)
          (fileType file)
          (path file)
      ]

mkFile :: String -> Text -> File
mkFile path fileType = File {..}
