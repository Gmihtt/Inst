{-# LANGUAGE OverloadedStrings #-}

module APP.Telegram.Buttons (login, run, stop, payment, statistics) where

import Types.Telegram.Types.Keyboard.InlineKeyboardButton
  ( InlineKeyboardButton,
    mkInlineKeyboardButton,
  )

login :: InlineKeyboardButton
login = mkInlineKeyboardButton "login" Nothing (Just "login")

run :: InlineKeyboardButton
run = mkInlineKeyboardButton "run" Nothing (Just "run")

stop :: InlineKeyboardButton
stop = mkInlineKeyboardButton "stop" Nothing (Just "stop")

payment :: InlineKeyboardButton
payment = mkInlineKeyboardButton "payment" Nothing (Just "payment")

statistics :: InlineKeyboardButton
statistics = mkInlineKeyboardButton "statistics" Nothing (Just "statistics")