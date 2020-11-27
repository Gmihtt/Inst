{-# LANGUAGE OverloadedStrings #-}
module APP.App where

import Configs.Config (getToken)
import Data.Text ( Text, pack)

app :: Text -> IO ()
app token = pure ()