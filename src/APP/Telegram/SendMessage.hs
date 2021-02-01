module APP.Telegram.SendMessage where

import API.CallTelegram (callTelegram)
import qualified API.Routes as API
import qualified Common.Environment as Environment
import Common.Error (throwTelegramErr)
import Common.Flow (Flow)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Types.Telegram.Methods.SendMessage (ReplyMarkup (..), mkSendMessage)
import Types.Telegram.Response (Response (..))
import Types.Telegram.Types.Keyboard.InlineKeyboardMarkup
  ( InlineKeyboardMarkup,
  )
import qualified Types.Telegram.Types.Message as Message

sendMessage :: Maybe InlineKeyboardMarkup -> Message.Message -> Flow (Response Message.Message)
sendMessage message keyboard = do
  env <- ask
  let eToken = Environment.token env
  let sM = mkSendMessage keyboard message
  callTelegram (API.sendMessage eToken sM)
