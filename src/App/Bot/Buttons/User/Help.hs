{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.Help where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

helpKeyboard :: InlineKeyboardMarkup
helpKeyboard = 
    mkInlineKeyboardMarkup [[mkButton "Back"]]