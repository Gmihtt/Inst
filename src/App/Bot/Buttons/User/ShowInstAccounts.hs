{-# LANGUAGE OverloadedStrings #-}

module App.Bot.Buttons.User.ShowInstAccounts
  ( instAccsKeyboard,
  )
where

import Data.Text (Text)
import Telegram.Types.Domain.Keyboard.InlineKeyboardButton
  ( mkButton,
  )
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
    mkInlineKeyboardMarkup,
  )

instAccsKeyboard :: [Text] -> InlineKeyboardMarkup
instAccsKeyboard instAccs =
  mkInlineKeyboardMarkup mkKeyboard
  where
    mkKeyboard = groupByThree (map mkButton instAccs) [] ++ [[mkButton "Add", mkButton "Back"]]

groupByThree :: [a] -> [a] -> [[a]]
groupByThree [] buf = [buf]
groupByThree (x : xs) buf
  | length buf >= 3 = buf : groupByThree xs [x]
  | otherwise = groupByThree xs (x : buf)
