{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.Admin.AdminMenu
  ( adminMenuKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

adminMenuKeyboard :: InlineKeyboardMarkup
adminMenuKeyboard =
  mkInlineKeyboardMarkup
    [ [mkButton "Управление пользователями"],
      [mkButton "Управление администраторами"],
      [mkButton "Вернуться в меню пользователя"]
    ]
