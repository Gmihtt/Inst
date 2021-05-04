{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.ProxyLoad where

import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import MongoDB.Transforms.Proxy (mkDocByProxy, mkProxyByDoc)
import Types.Domain.ProxyLoad (ProxyLoad (..))
import Prelude hiding (id)

mkDocByProxyLoad :: ProxyLoad -> Document
mkDocByProxyLoad ProxyLoad {..} =
  [ "proxy" =: mkDocByProxy proxy,
    "load" =: Int32 load
  ]

mkProxyLoadByDoc :: Document -> Maybe ProxyLoad
mkProxyLoadByDoc doc = do
  proxy <- mkProxyByDoc =<< doc !? "proxy"
  load <- doc !? "load"
  pure $ ProxyLoad {..}
