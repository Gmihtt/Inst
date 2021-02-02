module Types.Domain.Statistic where

import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

data Statistic 
  = Statistic {
    users :: Set.Set Text,
    lastUsers :: Seq.Seq Text
  } deriving (Show)


empty :: Statistic
empty = 
  Statistic {
    users = Set.empty,
    lastUsers = Seq.empty
  }

addUser :: Text -> Statistic -> Statistic 
addUser user stat@(Statistic users lastUsers)
  | Set.member user users = stat
  | otherwise =
  Statistic {
    users = Set.insert user users,
    lastUsers = fixLastUsers |> user
  }
  where
    fixLastUsers = 
      if Seq.length lastUsers == 1000
        then Seq.deleteAt 999 lastUsers
        else lastUsers

getSize :: Statistic -> Int
getSize (Statistic users _) = Set.size users

isMember :: Text -> Statistic -> Bool
isMember user stat@(Statistic users _) = Set.member user users

initWithLastUsers :: [Text] -> Statistic
initWithLastUsers users = 
  Statistic {
    users = Set.fromList users ,
    lastUsers = Seq.fromList users 
  }