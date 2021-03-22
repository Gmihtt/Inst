module App.Bot.Execution.Users.Statistics.Subscription where

import qualified App.Bot.Messages.FlowMessages as Messages
import Common.Flow (Flow)
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message

execute :: Message.Message -> Flow (Response Message.Message)
execute = Messages.todoMsg
