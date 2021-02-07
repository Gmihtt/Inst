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
  )
where

import Data.Text (Text, pack)
import Telegram.Types.Domain.Message (Message, mkMessage)

mainMenu :: Message -> Message
mainMenu =
  mkMessage $
    "Это базовое меню, вы можете перейти к выбору перейти выбрать инстаграмм аккаунты или попросить помощь"

login :: Message -> Message
login =
  mkMessage $
    "Пожалуйста отключите двухступенчатую аутентификацию и введите логин от инстаграм аккаунта"

password :: Message -> Message
password =
  mkMessage $
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
  mkMessage ("Статистика по вашему инстаграм аккаунту : " <> (pack $ show num)) msg

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