{-# LANGUAGE DeriveGeneric #-}

module Types.Telegram.Types.Message
  ( Message (..),
  )
where

import Common.Json
  ( FromJSON (..),
    ToJSON (..),
    Value,
    parseJson,
    toJson,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Telegram.Types.Chat (Chat)
import Types.Telegram.Types.MessageEntity (MessageEntities)
import Types.Telegram.Types.User (User)

type Time = Int

data Message
  = Message
      { message_id :: Int,
        from :: Maybe User,
        date :: Time,
        chat :: Chat,
        forward_from :: Maybe User,
        forward_date :: Maybe Time,
        reply_to_message :: Maybe Message,
        text :: Maybe Text,
        entities :: Maybe MessageEntities,
        audio :: Maybe Value,
        document :: Maybe Value,
        photo :: Maybe Value,
        sticker :: Maybe Value,
        video :: Maybe Value,
        voice :: Maybe Value,
        caption :: Maybe Text,
        contact :: Maybe Value,
        location :: Maybe Value,
        venue :: Maybe Value,
        new_chat_member :: Maybe User,
        left_chat_member :: Maybe User,
        new_chat_title :: Maybe Text,
        new_chat_photo :: Maybe Value,
        delete_chat_photo :: Maybe Bool,
        group_chat_created :: Maybe Bool,
        supergroup_chat_created :: Maybe Bool,
        channel_chat_created :: Maybe Bool,
        migrate_to_chat_id :: Maybe Int,
        migrate_from_chat_id :: Maybe Int,
        pinned_message :: Maybe Message
      }
  deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON = toJson

instance FromJSON Message where
  parseJSON = parseJson
