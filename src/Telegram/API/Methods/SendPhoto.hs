module Telegram.API.Methods.SendPhoto where

import qualified Common.Environment as Environment
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant.Multipart (genBoundary)
import Telegram.API.Methods.CallTelegram (callTelegram)
import qualified Telegram.API.Routes as API
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Methods.SendPhoto (mkPhoto)

sendPhoto :: Text -> FilePath -> Text -> Flow (Response Message.Message)
sendPhoto chatId filePath photoType = do
  env <- getEnvironment
  let eToken = Environment.token env
  let photo = mkPhoto filePath photoType
  boundary <- liftIO genBoundary
  callTelegram (API.sendPhoto chatId eToken (boundary, photo))
