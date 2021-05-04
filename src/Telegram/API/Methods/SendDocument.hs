module Telegram.API.Methods.SendDocument where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant.Multipart (genBoundary)
import Telegram.API.Methods.CallTelegram (callTelegram)
import qualified Telegram.API.Routes as API
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Methods.SendDocument (mkFile)

sendDocument :: Text -> FilePath -> Text -> Flow (Response Message.Message)
sendDocument chatId filePath fileType = do
  env <- getEnvironment
  let eToken = Environment.token env
  let file = mkFile filePath fileType
  boundary <- liftIO genBoundary
  callTelegram (API.sendDocument chatId eToken (boundary, file))
