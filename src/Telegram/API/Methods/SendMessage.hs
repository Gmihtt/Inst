module Telegram.API.Methods.SendMessage where

import qualified Common.Environment as Environment
import Common.Error (throwTelegramErr)
import Common.Flow (Flow)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Telegram.API.Methods.CallTelegram (callTelegram)
import qualified Telegram.API.Routes as API
import Telegram.Types.Communication.Response (Response (..))
import Telegram.Types.Domain.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
  )
import qualified Telegram.Types.Domain.Message as Message
import Telegram.Types.Methods.SendMessage (ReplyMarkup (..), mkSendMessage)

sendMessage :: Maybe InlineKeyboardMarkup -> Message.Message -> Flow (Response Message.Message)
sendMessage message keyboard = do
  env <- ask
  let eToken = Environment.token env
  let sM = mkSendMessage keyboard message
  callTelegram (API.sendMessage eToken sM)
