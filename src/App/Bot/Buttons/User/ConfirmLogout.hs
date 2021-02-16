{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.ConfirmLogout
  ( confirmLogout,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

confirmLogout :: InlineKeyboardMarkup
confirmLogout =
  mkInlineKeyboardMarkup
    [ [mkButton "Yes", mkButton "No"]
    ]
