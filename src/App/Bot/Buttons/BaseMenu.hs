{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.BaseMenu
  ( baseKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( InlineKeyboardButton,
    mkInlineKeyboardButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

baseKeyboard :: InlineKeyboardMarkup
baseKeyboard = mkInlineKeyboardMarkup [[login, run, stop, payment, statistics]]

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
