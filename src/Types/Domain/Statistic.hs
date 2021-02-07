module Types.Domain.Statistic where

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
getSize (Statistic stat_users _) = Set.size stat_users

isMember :: Text -> Statistic -> Bool
isMember user (Statistic stat_users _) = Set.member user stat_users

initWithLastUsers :: [Text] -> Statistic
initWithLastUsers stat_users =
  Statistic
    { users = Set.fromList stat_users,
      lastUsers = Seq.fromList stat_users
    }
