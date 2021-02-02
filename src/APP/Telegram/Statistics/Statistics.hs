module APP.Telegram.Statistics.Statistics where

import Common.Flow (Flow)
import qualified Types.Telegram.Types.Message as Message
import Types.Telegram.Response (Response (..))
import APP.Scripts.Statistics.API (sendMsg)

start :: Message.Message -> Int -> Flow (Response Message.Message)
start msg userId = 