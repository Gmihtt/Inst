module App.Bot.BotMain where

import qualified App.Bot.GetUpdates as GetUpdates
import qualified App.Bot.ParseUpdates as ParseUpdates
import qualified Common.Environment as Environment
import Common.Error (printDebug)
import Common.Flow (Flow, getEnvironment)
import Control.Monad.IO.Class (liftIO)
import qualified MongoDB.Queries.ProxyLoad as Mongo
import Services.CheckIP.CallCheckIP (checkIP)
import qualified Services.Proxy.GetAllProxy as GetAllProxy
import qualified Types.Communication.Proxy.ListOfProxy as ListOfProxy
import qualified Types.Domain.ProxyLoad as ProxyLoad
import qualified Types.Domain.ProxyStatus as ProxyStatus

run :: Maybe Integer -> Flow ()
run updateId = do
  updateProxyLoad
  updates <- GetUpdates.execute updateId
  ParseUpdates.execute updates

updateProxyLoad :: Flow ()
updateProxyLoad = do
  allProxy <- GetAllProxy.getAllProxy
  checkedProxy <- mapM checkIP $ ListOfProxy.results allProxy
  let goodProxy = map fst $ filter snd checkedProxy
  proxyLoad <- Mongo.getAllProxyLoad
  let oldProxy = map ProxyLoad.proxy proxyLoad
  let newProxy = filter (`notElem` oldProxy) goodProxy
  let newProxyPayload = map ProxyLoad.mkProxyLoadByProxy newProxy
  env <- getEnvironment
  let proxyManager = Environment.proxyManager env
  let listOfProxy = newProxyPayload ++ proxyLoad
  liftIO $ printDebug listOfProxy
  liftIO $ ProxyStatus.addProxyLoads listOfProxy proxyManager
  Mongo.insertManyProxyLoad $ map ProxyLoad.mkProxyLoadByProxy newProxy
