module App.Bot.Execution.Admin.SelectUser where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import Data.Text ( Text )
import Common.Error (throwLogicError)
import qualified Common.Redis as Common
import qualified Common.TelegramUserStatus as Common
import qualified MongoDB.Queries.Accounts as Mongo
import Control.Monad.IO.Class (liftIO)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Types.Domain.InstAccount as InstAccount
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.TgUser as TgUser
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

showOneUser = undefined
showAllUsers = undefined