{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.AccountMenu
  ( accountMenuKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

accountMenuKeyboard :: InlineKeyboardMarkup
accountMenuKeyboard = 
    mkInlineKeyboardMarkup 
      [ [mkButton "Start", mkButton "Stop", mkButton "Statistics"],
        [mkButton "Subscription"], [mkButton "Logout"],
        [mkButton "Back"]
      ]