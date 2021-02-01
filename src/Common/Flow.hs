module Common.Flow where

import Common.Environment (Environment)
import Control.Monad.Trans.Reader (ReaderT)

type Flow a = ReaderT Environment IO a
