module App.Bot.Messages.FlowMessages
  ( baseMenu,
    oldMsg,
    msgForEmptyUser,
    failAuthMsg,
    loginMsg,
    successAuthMsg,
    repeatLoggingMsg,
    sendStat,
    sendEmptyStat,
  )
where

import App.Bot.Buttons.BaseMenu (baseKeyboard)
import qualified App.Bot.Messages.MessagesBody as Messages
import Telegram.API.Methods.SendMessage (sendMessage)
import Common.Flow (Flow)
import Data.Text (empty)
import Telegram.Types.Communication.Response (Response (..))
import Telegram.Types.Domain.Message (Message)

baseMenu :: Message -> Flow (Response Message)
baseMenu msg = sendMessage (Just baseKeyboard) (Messages.baseMenu msg)

oldMsg :: Message -> Flow (Response Message)
oldMsg msg = do
  sendMessage Nothing (Messages.oldMsg msg)
  baseMenu msg

msgForEmptyUser :: Message -> Flow (Response Message)
msgForEmptyUser msg = do
  sendMessage Nothing (Messages.emptyUser msg)
  baseMenu msg

failAuthMsg :: Message -> Flow (Response Message)
failAuthMsg msg = do
  sendMessage Nothing (Messages.failAuth empty msg)
  baseMenu msg

loginMsg :: Message -> Flow (Response Message)
loginMsg msg = do
  sendMessage Nothing (Messages.login msg)

successAuthMsg :: Message -> Flow (Response Message)
successAuthMsg msg = do
  sendMessage Nothing (Messages.success msg)
  baseMenu msg

repeatLoggingMsg :: Message -> Flow (Response Message)
repeatLoggingMsg msg = do
  sendMessage Nothing (Messages.repeatLogging msg)
  baseMenu msg

sendStat :: Message -> Int -> Flow (Response Message)
sendStat msg stat = do
  sendMessage Nothing (Messages.stat msg stat)
  baseMenu msg

sendEmptyStat :: Message -> Flow (Response Message)
sendEmptyStat msg = do
  sendMessage Nothing (Messages.emptyStat msg)
  baseMenu msg
