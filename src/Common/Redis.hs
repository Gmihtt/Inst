{-# LANGUAGE OverloadedStrings #-}

module Common.Redis where

import Common.Flow (Flow)
import qualified Data.Text as T
import qualified MongoDB.Queries.Accounts as Mongo
import qualified Redis.Queries as Redis
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser

getInstAccsByTgUserId :: Int -> Flow [InstAccount.InstAccount]
getInstAccsByTgUserId userId = do
  mbInstAccs <- Redis.getValue userId
  maybe (putInstAccsByTgUserId userId >> getInstAccsByTgUserId userId) pure mbInstAccs

putInstAccsByTgUserId :: Int -> Flow ()
putInstAccsByTgUserId userId = do
  let tgId = TgUser.TgId . T.pack $ show userId
  instAccs <- Mongo.findInstAccsByTgId tgId
  Redis.putValue userId instAccs

dropInstAccs :: Int -> Flow ()
dropInstAccs = Redis.deleteValue
