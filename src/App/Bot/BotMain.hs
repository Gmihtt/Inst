module App.Bot.BotMain where

import qualified App.Bot.GetUpdates as GetUpdates
import qualified App.Bot.ParseUpdates as ParseUpdates
import Common.Flow (Flow)

run :: Maybe Integer -> Flow ()
run updateId = do
  updates <- GetUpdates.execute updateId
  ParseUpdates.execute updates
