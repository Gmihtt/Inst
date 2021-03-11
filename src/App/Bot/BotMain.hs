module App.Bot.BotMain where

import qualified App.Bot.GetUpdates as GetUpdates
import qualified App.Bot.ParseUpdates as ParseUpdates
import qualified Common.Environment as Environment

run :: Maybe Integer -> Environment.Environment -> IO ()
run updateId env = do
  updates <- GetUpdates.execute updateId env
  ParseUpdates.execute updates env
