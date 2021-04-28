{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.Confirm
  ( confirm,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

confirm :: InlineKeyboardMarkup
confirm =
  mkInlineKeyboardMarkup
    [ [mkButton "Yes", mkButton "No"]
    ]
