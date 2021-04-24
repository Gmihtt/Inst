{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.Admin.SelectUser
  ( selectUserKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

selectUserKeyboard :: InlineKeyboardMarkup
selectUserKeyboard =
  mkInlineKeyboardMarkup
    [ [mkButton "Найти по telegram"],
      [mkButton "Найти по instagram"],
      [mkButton "Назад"]
    ]
