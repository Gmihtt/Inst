{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Statistics where

import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Data.Text (Text)

data Statistics
  = Statistics
      { users :: Set.Set Text,
        lastUsers :: Seq.Seq Text
      }
  deriving (Show)

empty :: Statistics
empty =
  Statistics
    { users = Set.empty,
      lastUsers = Seq.empty
    }

addUser :: Text -> Statistics -> Statistics
addUser user stat@(Statistics stat_users stat_lastUsers)
  | Set.member user stat_users = stat
  | isJust (Seq.elemIndexL user stat_lastUsers) = stat
  | otherwise =
    Statistics
      { users = Set.insert user stat_users,
        lastUsers = fixLastUsers |> user
      }
  where
    fixLastUsers =
      if Seq.length stat_lastUsers == 1000
        then Seq.deleteAt 999 stat_lastUsers
        else stat_lastUsers

getSize :: Statistics -> Int
getSize Statistics {..} = Set.size users

getLastUsers :: Statistics -> [Text]
getLastUsers Statistics {..} = toList lastUsers

isMember :: Text -> Statistics -> Bool
isMember user Statistics {..} = Set.member user users

initWithLastUsers :: [Text] -> Statistics
initWithLastUsers lastCountUsers =
  Statistics
    { users = Set.empty,
      lastUsers = Seq.fromList lastCountUsers
    }
