{-# LANGUAGE OverloadedStrings #-}

module Common.Redis where

import Common.Flow (Flow)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified Redis.Queries as Redis
import qualified Types.Domain.InstAccount as InstAccount

getInstAccs :: Int -> Flow [InstAccount.InstAccount]
getInstAccs userId = do
  mbInstAccs <- Redis.getValue userId
  maybe (putInstAccs userId >> getInstAccs userId) pure mbInstAccs

putInstAccs :: Int -> Flow ()
putInstAccs userId = do
  let uId = T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId uId
  Redis.putValue userId instAccs

dropInstAccs :: Int -> Flow ()
dropInstAccs = Redis.deleteValue
