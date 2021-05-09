{-# LANGUAGE OverloadedStrings #-}

module App.Scripts.Statistics.API where

import Common.Error
  ( printDebug,
    printDebug,
    throwSocketErr,
  )
import qualified Communication.Sockets.API as SocketAPI
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import qualified Types.Communication.Error as Error
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Communication.Scripts.Statistics.Response as ResponseStat
import qualified Types.Domain.Statistic as Statistic
import qualified Types.Domain.ThreadManager as Manager

statConnection :: SocketAPI.Socket -> IO Manager.StatisticsManager
statConnection socket = do
  liftIO $ SocketAPI.runConnection socket getUsername mkStatistics
  where
    getUsername bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . ResponseStat.inst_id) (decode bsBody)

mkStatistics :: ByteString -> Maybe (Either Error.Error Statistic.Statistic) -> IO (Either Error.Error Statistic.Statistic)
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  eUsers <- getUsers value
  case eUsers of
    Right users ->
      pure $ maybe (Right $ addUsers users Statistic.empty) (fmap (addUsers users)) mbStat
    Left err -> pure $ Left err
  where
    addUsers users stat = foldr Statistic.addUser stat users
    getUsers value =
      let mbError = ResponseStat.error value
       in pure $ maybe (Right . fromMaybe [] $ ResponseStat.users value) Left mbError

sendMsg :: Manager.StatisticsManager -> RequestStat.Request -> IO ()
sendMsg manager req = do
  printDebug req
  Manager.sendMsg (encode req) manager
