{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Usernames where

import Data.Text (Text)
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.TgUser as TgUser

data Usernames
  = Usernames
      { instUsername :: InstAccount.InstUsername,
        instId :: InstAccount.InstId,
        tgUsername :: Maybe TgUser.TgUsername,
        tgId :: TgUser.TgId
      }
  deriving (Show, Eq)

mkUsernames :: InstAccount.InstUsername -> InstAccount.InstId -> Maybe TgUser.TgUsername -> TgUser.TgId -> Usernames
mkUsernames instUsername instId tgUsername tgId =
  Usernames
    { ..
    }
