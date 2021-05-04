{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Communication.Error where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data ErrorCode
  = USER_IS_NOT_LOGGED
  | FETCHING_ERROR
  | NO_USER_DIR
  | LOGOUT_FAILURE
  | LOGOUT_NO_USER
  | OTHER_ERROR_1
  | OTHER_ERROR_2
  deriving (Show, Eq, Generic)

instance ToJSON ErrorCode where
  toJSON = toJson

instance FromJSON ErrorCode where
  parseJSON = parseJson

data Error
  = Error
      { error_message :: Text,
        error_code :: ErrorCode
      }
  deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON = toJson

instance FromJSON Error where
  parseJSON = parseJson

parseCriticalError :: ErrorCode -> Maybe Text
parseCriticalError USER_IS_NOT_LOGGED = Just "Вас разлогинило, сделайте логаут и зайдите заново, либо обратитесь к администратору"
parseCriticalError NO_USER_DIR = Just "Произошла ошибка со стороны приложения, пожалуйста обратитесь к администратору"
parseCriticalError _ = Nothing
