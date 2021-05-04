{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MongoDB.Transforms.Proxy where

import Database.MongoDB
  ( (!?),
    (=:),
    Document,
    Value (..),
  )
import Types.Domain.Proxy (Proxy (..))
import Prelude hiding (id)

mkDocByProxy :: Proxy -> Document
mkDocByProxy Proxy {..} =
  [ "ip" =: String ip,
    "port_http" =: String port_http,
    "username" =: String username,
    "password" =: String password
  ]

mkProxyByDoc :: Document -> Maybe Proxy
mkProxyByDoc doc = do
  ip <- doc !? "ip"
  port_http <- doc !? "port_http"
  username <- doc !? "first_name"
  password <- doc !? "password"
  pure $ Proxy {..}
