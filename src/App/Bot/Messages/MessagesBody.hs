{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Messages.MessagesBody
  ( baseMenu,
    login,
    success,
    failAuth,
    emptyUser,
    repeatLogging,
    oldMsg,
    stat,
    emptyStat,
  )
where

import Data.Text (Text, pack)
import Telegram.Types.Domain.Message (Message, mkMessage)

baseMenu :: Message -> Message
baseMenu =
  mkMessage $
    "Это базовое меню, с его помощью вы можете авторизоваться"
      <> "оплатить подписку, запустить или остановить анализ вашего инстаграм аккаунта"
      <> "а так же вывести статистику"

login :: Message -> Message
login =
  mkMessage $
    "Пожалуйста отключите двухступенчатую аутентификацию и введите логин инстаграм аккаунта\n"
      <> "В первой строке введите логин, во второй пароль"

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

emptyStat :: Message -> Message
emptyStat =
  mkMessage "Вы не указали ещё не один истаграм аккаунт"