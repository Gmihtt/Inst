module App.Bot.Execution.Admin.AdminMenu where

import qualified App.Bot.Messages.FlowMessages as Message
import Common.Flow (Flow)
import qualified Common.TelegramUserStatus as Common
import qualified Data.Text as T
import qualified MongoDB.Queries.ProxyLoad as ProxyLoad
import Telegram.Types.Communication.Response (Response (..))
import qualified Telegram.Types.Domain.Message as Message
import qualified Telegram.Types.Domain.User as User
import qualified Types.Domain.Proxy as Proxy
import qualified Types.Domain.ProxyLoad as ProxyLoad
import qualified Types.Domain.Status.TgUserStatus as TgUserStatus

selectUser :: Message.Message -> User.User -> Flow (Response Message.Message)
selectUser msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.SelectTgUser
  Common.updateUserStatus user status
  Message.selectUser msg

selectAdmin :: Message.Message -> User.User -> Flow (Response Message.Message)
selectAdmin msg user = do
  let status = TgUserStatus.TgAdmin TgUserStatus.SelectAdmin
  Common.updateUserStatus user status
  Message.selectAdmin msg

proxyLoad :: Message.Message -> Flow (Response Message.Message)
proxyLoad msg = do
  allProxyLoad <- ProxyLoad.getAllProxyLoad
  let proxyLoads = map (\pl -> (Proxy.ip $ ProxyLoad.proxy pl, T.pack . show $ ProxyLoad.load pl)) allProxyLoad
  Message.proxyLoad proxyLoads msg

back :: Message.Message -> User.User -> Flow (Response Message.Message)
back msg user = do
  let status = TgUserStatus.TgUser TgUserStatus.MainMenu
  Common.updateUserStatus user status
  Message.mainMenu msg
