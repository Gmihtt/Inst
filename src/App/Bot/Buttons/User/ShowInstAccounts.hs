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
    mkKeyboard = fun (map mkButton instAccs) [] ++ [[mkButton "Add", mkButton "Back"]]

fun :: [a] -> [a] -> [[a]]
fun [] buf = [buf]
fun (x : xs) buf
  | length buf >= 3 = buf : fun xs [x]
  | otherwise = fun xs (x : buf)
