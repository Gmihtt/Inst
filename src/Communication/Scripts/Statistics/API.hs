{-# LANGUAGE OverloadedStrings #-}

module Communication.Scripts.Statistics.API where

import Common.Error
  ( printDebug,
    throwSocketErr,
  )
import qualified Communication.Scripts.MessagesHandler.API as MessagesHandlerAPI
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import qualified Types.Communication.Scripts.Error as Error
import qualified Types.Communication.Scripts.Statistics.Request as RequestStat
import qualified Types.Communication.Scripts.Statistics.Response as ResponseStat
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Socket as Socket
import qualified Types.Domain.Statistics as Statistics

type Msg = (Either Error.Error Statistics.Statistics)

type StatisticsMessagesHandler = MessagesHandlerAPI.MessagesHandler InstAccount.InstId Msg

statConnection :: Socket.Socket -> IO StatisticsMessagesHandler
statConnection socket = do
  liftIO $ MessagesHandlerAPI.runMessagesHandler socket getInstId mkStatistics
  where
    getInstId bsBody =
      maybe (throwSocketErr $ "decode fail" <> show bsBody) (pure . InstAccount.InstId . ResponseStat.inst_id) (decode bsBody)

mkStatistics ::
  ByteString ->
  Maybe Msg ->
  IO Msg
mkStatistics bsBody mbStat = do
  value <- maybe (throwSocketErr $ "decode fail" <> show bsBody) pure (decode bsBody)
  printDebug value
  eUsers <- getUsers value
  case eUsers of
    Right users ->
      pure $ maybe (Right $ addUsers users Statistics.empty) (fmap (addUsers users)) mbStat
    Left err -> pure $ Left err
  where
    addUsers users stat = foldr Statistics.addUser stat users
    getUsers value =
      let mbError = ResponseStat.error value
       in pure $ maybe (Right . fromMaybe [] $ ResponseStat.users value) Left mbError

sendMsg :: StatisticsMessagesHandler -> RequestStat.Request -> IO ()
sendMsg handler req = do
  printDebug req
  MessagesHandlerAPI.sendMsg (encode req) handler

getCurrentStatistics :: InstAccount.InstId -> StatisticsMessagesHandler -> IO (Maybe Msg)
getCurrentStatistics = MessagesHandlerAPI.findMsg

deleteStatistics :: InstAccount.InstId -> StatisticsMessagesHandler -> IO Bool
deleteStatistics = MessagesHandlerAPI.deleteMsg

addStatistics :: InstAccount.InstId -> Msg -> StatisticsMessagesHandler -> IO Bool
addStatistics = MessagesHandlerAPI.addMsg
