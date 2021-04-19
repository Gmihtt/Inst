{-# LANGUAGE RecordWildCards #-}

module Types.Domain.Statistic where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Data.Text (Text)

data Statistic
  = Statistic
      { users :: Set.Set Text,
        lastUsers :: Seq.Seq Text
      }
  deriving (Show)

empty :: Statistic
empty =
  Statistic
    { users = Set.empty,
      lastUsers = Seq.empty
    }

addUser :: Text -> Statistic -> Statistic
addUser user stat@(Statistic stat_users stat_lastUsers)
  | Set.member user stat_users = stat
  | otherwise =
    Statistic
      { users = Set.insert user stat_users,
        lastUsers = fixLastUsers |> user
      }
  where
    fixLastUsers =
      if Seq.length stat_lastUsers == 1000
        then Seq.deleteAt 999 stat_lastUsers
        else stat_lastUsers

getSize :: Statistic -> Int
getSize Statistic {..} = Set.size users

getLastUsers :: Statistic -> [Text]
getLastUsers Statistic {..} = toList lastUsers

isMember :: Text -> Statistic -> Bool
isMember user Statistic {..} = Set.member user users

initWithLastUsers :: [Text] -> Statistic
initWithLastUsers lastCountUsers =
  Statistic
    { users = Set.fromList lastCountUsers,
      lastUsers = Seq.fromList lastCountUsers
    }
