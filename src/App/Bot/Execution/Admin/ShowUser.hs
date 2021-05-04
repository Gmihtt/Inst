module App.Bot.Execution.Admin.ShowUser where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Error (throwLogicError)
import Common.Flow (Flow)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified MongoDB.Queries.Accounts as Mongo
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.InstAccount as InstAccount
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus
import qualified Types.Domain.TgUser as TgUser

showOneUser = undefined

showAllUsers = undefined
