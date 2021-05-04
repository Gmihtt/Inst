{-# LANGUAGE OverloadedStrings #-}

module App.Bot.GetUpdates
  ( execute,
  )
where

import Common.Flow (Flow, getEnvironment, runFlow)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import Control.Monad.IO.Class (liftIO)
import Telegram.API.Methods.GetUpdates (getUpdates)
import Types.Domain.TgUpdates (ListOfUpdates)

execute :: Maybe Integer -> Flow ListOfUpdates
execute updateId = do
  env <- getEnvironment
  listOfUpdates <- liftIO newChan
  liftIO . forkIO $ runFlow (getUpdates updateId listOfUpdates) env
  pure listOfUpdates
