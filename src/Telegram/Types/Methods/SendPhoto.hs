{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.Methods.SendPhoto where

import Data.Text (Text, pack)
import Servant.Multipart

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
