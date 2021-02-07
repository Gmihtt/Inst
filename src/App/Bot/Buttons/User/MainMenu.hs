{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.MainMenu
  ( mainMenuKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

mainMenuKeyboard :: InlineKeyboardMarkup
mainMenuKeyboard =
  mkInlineKeyboardMarkup [[mkButton "Accounts", mkButton "Help"]]
