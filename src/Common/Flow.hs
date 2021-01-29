module Common.Flow where

import Control.Monad.Trans.Reader (ReaderT)
import Common.Environment ( Environment ) 

type Flow a = ReaderT Environment IO a