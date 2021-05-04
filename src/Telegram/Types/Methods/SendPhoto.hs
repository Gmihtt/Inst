{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.SendPhoto where

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

data Photo = Photo {path :: FilePath, photoType :: Text}

instance ToMultipart Tmp Photo where
  toMultipart file =
    MultipartData
      []
      [ FileData
          ""
          (pack $ path file)
          (photoType file)
          (path file)
      ]

mkPhoto :: String -> Text -> Photo
mkPhoto path photoType = Photo {..}
