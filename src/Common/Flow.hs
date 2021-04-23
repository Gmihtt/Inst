module Common.Flow where

import Common.Environment (Environment)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)

type Flow = ReaderT Environment IO

runFlow :: Flow a -> Environment -> IO a
runFlow = runReaderT

getEnvironment :: Flow Environment
getEnvironment = ask
