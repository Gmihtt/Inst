{-# LANGUAGE OverloadedStrings #-}
module App where

import Config (getToken)
import Data.Text ( Text, pack)

parseError :: String -> IO Text
parseError err = pure .  pack $ "error when parsing token : " <> err

app :: IO ()
app = do
  token <- getToken >>= either  parseError pure
  print token