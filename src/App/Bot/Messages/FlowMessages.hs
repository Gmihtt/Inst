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
    doubleAuth,
    susCode,
    incorrectCode,
    confirmLogout,
    logout,
    lastCountUsersNotFound,
    continueStat,
    choseStatistics,
    failInstIdOrPrivate,
    adminMenu,
    selectUser,
    selectAdmin,
    enterUsername,
    smthMessage,
    timeBlockMessage,
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

doubleAuth :: Message -> Flow (Response Message)
doubleAuth msg =
  sendMessage Nothing (Messages.doubleAuth msg)

susCode :: Message -> Flow (Response Message)
susCode msg =
  sendMessage Nothing (Messages.susCode msg)

incorrectCode :: Message -> Flow (Response Message)
incorrectCode msg =
  sendMessage Nothing (Messages.incorrectCode msg)

confirmLogout :: Message -> Flow (Response Message)
confirmLogout msg =
  sendMessage (Just Keyboard.confirm) (Messages.confirmLogout msg)

continueStat :: Message -> Flow (Response Message)
continueStat msg =
  sendMessage (Just Keyboard.confirm) (Messages.continueStat msg)

lastCountUsersNotFound :: Message -> Flow (Response Message)
lastCountUsersNotFound msg =
  sendMessage Nothing (Messages.lastCountUsersNotFound msg)

choseStatistics :: Message -> Flow (Response Message)
choseStatistics msg =
  sendMessage (Just Keyboard.choseStatisticsKeyboard) (Messages.choseStatistics msg)

failInstIdOrPrivate :: Message -> Flow (Response Message)
failInstIdOrPrivate msg =
  sendMessage Nothing (Messages.failInstIdOrPrivate msg)

logout :: Message -> Flow (Response Message)
logout msg =
  sendMessage Nothing (Messages.logout msg)

adminMenu :: Message -> Flow (Response Message)
adminMenu msg =
  sendMessage (Just Keyboard.adminMenuKeyboard) (Messages.adminMenu msg)

selectUser :: Message -> Flow (Response Message)
selectUser msg =
  sendMessage (Just Keyboard.selectUserKeyboard) (Messages.selectUser msg)

selectAdmin :: Message -> Flow (Response Message)
selectAdmin msg =
  sendMessage Nothing (Messages.selectAdmin msg)

enterUsername :: Message -> Flow (Response Message)
enterUsername msg =
  sendMessage Nothing (Messages.enterUsername msg)

smthMessage :: (Show a) => a -> Message -> Flow (Response Message)
smthMessage a msg =
  sendMessage Nothing (Messages.smthMessage a msg)

timeBlockMessage :: Int -> Message -> Flow (Response Message)
timeBlockMessage time msg =
  sendMessage Nothing (Messages.timeBlockMessage time msg)
