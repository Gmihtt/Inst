module Main where

import API.Routes ( run )
import Configs.Config (getToken)
import Control.Monad.Trans.Except ( runExceptT )
import Common.Errors (tokenError)

main :: IO ()
main = do
  eToken <- runExceptT getToken
  either tokenError run eToken

