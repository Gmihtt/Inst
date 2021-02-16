module App.Bot.Messages.FlowMessages
  ( mainMenu,
    oldMsg,
    msgForEmptyUser,
    failAuthMsg,
    loginMsg,
    passwordMsg,
    successAuthMsg,
    repeatLoggingMsg,
    sendStat,
    showInstAccs,
    strangeMessage,
    helpMessage,
    accountMenu,
    start,
    stop,
    todoMsg,
    publicAccount,
    authCode,
    incorrectAuthCode,
    confirmLogout,
    logout,
  )
where

import qualified App.Bot.Buttons.Keyboards as Keyboard
import qualified App.Bot.Messages.MessagesBody as Messages
import Common.Flow (Flow)
import Data.Text (Text)
import qualified Data.Text as Text
import Telegram.API.Methods.SendMessage (sendMessage)
import Telegram.Types.Communication.Response (Response (..))
import Telegram.Types.Domain.Message (Message)

mainMenu :: Message -> Flow (Response Message)
mainMenu msg = sendMessage (Just Keyboard.mainMenuKeyboard) (Messages.mainMenu msg)

oldMsg :: Message -> Flow (Response Message)
oldMsg msg = do
  sendMessage Nothing (Messages.oldMsg msg)

msgForEmptyUser :: Message -> Flow (Response Message)
msgForEmptyUser msg = do
  sendMessage Nothing (Messages.emptyUser msg)

failAuthMsg :: Message -> Flow (Response Message)
failAuthMsg msg = do
  sendMessage Nothing (Messages.failAuth Text.empty msg)

loginMsg :: Message -> Flow (Response Message)
loginMsg msg = do
  sendMessage Nothing (Messages.login msg)

passwordMsg :: Message -> Flow (Response Message)
passwordMsg msg = do
  sendMessage Nothing (Messages.password msg)

successAuthMsg :: Message -> Flow (Response Message)
successAuthMsg msg = do
  sendMessage Nothing (Messages.success msg)

repeatLoggingMsg :: Message -> Flow (Response Message)
repeatLoggingMsg msg = do
  sendMessage Nothing (Messages.repeatLogging msg)

sendStat :: Message -> Int -> Flow (Response Message)
sendStat msg stat = do
  sendMessage Nothing (Messages.stat msg stat)

showInstAccs :: Message -> [Text] -> Flow (Response Message)
showInstAccs msg instAccs =
  sendMessage
    (Just $ Keyboard.instAccsKeyboard instAccs)
    (Messages.showInstAccs msg)

strangeMessage :: Message -> Flow (Response Message)
strangeMessage msg =
  sendMessage Nothing (Messages.strangeMessage msg)

helpMessage :: Message -> Flow (Response Message)
helpMessage msg =
  sendMessage (Just Keyboard.helpKeyboard) (Messages.helpMessage msg)

accountMenu :: Message -> Flow (Response Message)
accountMenu msg =
  sendMessage (Just Keyboard.accountMenuKeyboard) (Messages.accountMenu msg)

start :: Message -> Flow (Response Message)
start msg =
  sendMessage Nothing (Messages.startMsg msg)

stop :: Message -> Flow (Response Message)
stop msg =
  sendMessage Nothing (Messages.stopMsg msg)

todoMsg :: Message -> Flow (Response Message)
todoMsg msg =
  sendMessage Nothing (Messages.todoMsg msg)

publicAccount :: Message -> Flow (Response Message)
publicAccount msg =
  sendMessage Nothing (Messages.publicAccount msg)

authCode :: Message -> Flow (Response Message)
authCode msg =
  sendMessage Nothing (Messages.authCode msg)

incorrectAuthCode :: Message -> Flow (Response Message)
incorrectAuthCode msg =
  sendMessage Nothing (Messages.incorrectAuthCode msg)

confirmLogout :: Message -> Flow (Response Message)
confirmLogout msg =
  sendMessage (Just Keyboard.confirmLogout) (Messages.confirmLogout msg)

logout :: Message -> Flow (Response Message)
logout msg =
  sendMessage Nothing (Messages.logout msg)
