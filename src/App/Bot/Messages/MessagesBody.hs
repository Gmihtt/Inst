{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Messages.MessagesBody
  ( mainMenu,
    login,
    password,
    success,
    failAuth,
    emptyUser,
    repeatLogging,
    oldMsg,
    stat,
    showInstAccs,
    strangeMessage,
    helpMessage,
    accountMenu,
    startMsg,
    stopMsg,
    todoMsg,
    publicAccount,
    doubleAuth,
    susCode,
    incorrectCode,
    confirmLogout,
    logout,
    continueStat,
    lastCountUsersNotFound,
    choseStatistics,
    failInstIdOrPrivate,
    adminMenu,
    selectUser,
    selectAdmin,
    enterUsername,
    smthMessage,
    timeBlockMessage,
    waitMessage,
  )
where

import Data.Text (Text, pack)
import Telegram.Types.Domain.Message (Message, mkMessage)

mainMenu :: Message -> Message
mainMenu =
  mkMessage
    "Это базовое меню, вы можете перейти к выбору перейти выбрать инстаграмм аккаунты или попросить помощь"

login :: Message -> Message
login =
  mkMessage
    "Введите логин от инстаграм аккаунта"

password :: Message -> Message
password =
  mkMessage
    "Введите пароль"

success :: Message -> Message
success =
  mkMessage "Успешно"

failAuth :: Text -> Message -> Message
failAuth text =
  mkMessage ("Не удалось получить доступ к вашему инстаграмм аккаунту. " <> text)

emptyUser :: Message -> Message
emptyUser =
  mkMessage "Ваш аккаунт скрыт, вам нужно поменять настройки"

repeatLogging :: Message -> Message
repeatLogging =
  mkMessage "Этот аккунт уже используется"

oldMsg :: Message -> Message
oldMsg =
  mkMessage "Прошло слишком много времени с момента отправки сообщения, давайте попробуем ещё раз"

stat :: Message -> Int -> Message
stat msg num =
  mkMessage ("Статистика по вашему инстаграм аккаунту : " <> pack (show num)) msg

showInstAccs :: Message -> Message
showInstAccs =
  mkMessage "Это список зарегестрированных аккаунтов, вы можете выбрать любой из них или добавить новый"

strangeMessage :: Message -> Message
strangeMessage =
  mkMessage "К сожалению, я не понимаю что вы от меня хотите :("

helpMessage :: Message -> Message
helpMessage =
  mkMessage "Если у вас возникли какие-то проблемы, обратитесь к @durahan"

accountMenu :: Message -> Message
accountMenu =
  mkMessage "Меню вашего аккаунта"

startMsg :: Message -> Message
startMsg =
  mkMessage "Начал собирать статистику"

stopMsg :: Message -> Message
stopMsg =
  mkMessage "Ок, останавливаюсь"

todoMsg :: Message -> Message
todoMsg =
  mkMessage "эта функция ещё не реализована"

doubleAuth :: Message -> Message
doubleAuth =
  mkMessage "Введите код от двухфакторной аутентификации"

susCode :: Message -> Message
susCode =
  mkMessage "Вам на почту или телефон должен был прийти код, введите код"

incorrectCode :: Message -> Message
incorrectCode =
  mkMessage "Либо вы прислали неверный код, либо попробуйте зарегестироваться позже"

publicAccount :: Message -> Message
publicAccount =
  mkMessage "Ваш аккаунт открытый, пока вы его не закроете не получится собирать статистику"

confirmLogout :: Message -> Message
confirmLogout =
  mkMessage "Подвертидте выход"

continueStat :: Message -> Message
continueStat =
  mkMessage "Вы желаете продолжить предыдущий сбор статистики?"

lastCountUsersNotFound :: Message -> Message
lastCountUsersNotFound =
  mkMessage "Не удалось найти данные о вашем прошлом сборе статистики"

choseStatistics :: Message -> Message
choseStatistics =
  mkMessage "Выберите какую статистику хотитет получить"

failInstIdOrPrivate :: Message -> Message
failInstIdOrPrivate =
  mkMessage "Не удалось получить данные вашего инстаграмм аккаунта"

logout :: Message -> Message
logout =
  mkMessage "Вы успешно вышли из аккаунта"

adminMenu :: Message -> Message
adminMenu =
  mkMessage "Это базовое меню администратор"

selectUser :: Message -> Message
selectUser =
  mkMessage "Если пользователь зарегестирован в базе, то вы можете найти его по его username в telegram или instagram"

selectAdmin :: Message -> Message
selectAdmin =
  mkMessage "Выберете администратора по его telegram username"

enterUsername :: Message -> Message
enterUsername =
  mkMessage "Введите username"

smthMessage :: (Show a) => a -> Message -> Message
smthMessage a =
  mkMessage (pack $ show a)

timeBlockMessage :: Int -> Message -> Message
timeBlockMessage time =
  mkMessage $ "В сможете зарегестрироваться не раньше чем через " <> pack (show time) <> " минут"

waitMessage :: Message -> Message
waitMessage =
  mkMessage "Может пройти до 5 минут, пока вы сможете зарегестироваться"
