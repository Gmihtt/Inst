{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.SendDocument where

import Data.Text (Text, pack)
import Servant.Multipart

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
