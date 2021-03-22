{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.ChoseStatistics
  ( choseStatisticsKeyboard,
  )
where

import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

choseStatisticsKeyboard :: InlineKeyboardMarkup
choseStatisticsKeyboard =
  mkInlineKeyboardMarkup
    [ [mkButton "Current"],
      [mkButton "Day"],
      [mkButton "7 Days"],
      [mkButton "30 Days"]
    ]
