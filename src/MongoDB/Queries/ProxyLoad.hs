{-# LANGUAGE OverloadedStrings #-}

module MongoDB.Queries.ProxyLoad where

import Common.Flow (Flow)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import qualified MongoDB.Queries.Common as QMongo
import qualified MongoDB.Transforms.Proxy as Transforms
import qualified MongoDB.Transforms.ProxyLoad as Transforms
import qualified Types.Domain.Proxy as Proxy
import qualified Types.Domain.ProxyLoad as ProxyLoad

collectionName :: Text
collectionName = "proxy_load"

insertManyProxyLoad :: [ProxyLoad.ProxyLoad] -> Flow ()
insertManyProxyLoad = QMongo.insertMany collectionName . map Transforms.mkDocByProxyLoad

updateProxyLoad :: ProxyLoad.ProxyLoad -> Flow ()
updateProxyLoad proxyLoad = do
  let dcProxy = Transforms.mkDocByProxy $ ProxyLoad.proxy proxyLoad
  QMongo.upsert (Mongo.select ["proxy" =: dcProxy] collectionName) (Transforms.mkDocByProxyLoad proxyLoad)

findOneProxyLoadByProxy :: Proxy.Proxy -> Flow (Maybe ProxyLoad.ProxyLoad)
findOneProxyLoadByProxy proxy = do
  res <- QMongo.findOne (Mongo.select (Transforms.mkDocByProxy proxy) collectionName)
  pure $ Transforms.mkProxyLoadByDoc =<< res

getAllProxyLoad :: Flow [ProxyLoad.ProxyLoad]
getAllProxyLoad = do
  res <- QMongo.find (Mongo.select [] collectionName)
  pure $ mapMaybe Transforms.mkProxyLoadByDoc res
