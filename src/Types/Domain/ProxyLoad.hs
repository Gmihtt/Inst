{-# LANGUAGE RecordWildCards #-}

module Types.Domain.ProxyLoad where

import Data.Int (Int32)
import Types.Domain.Proxy (Proxy)

data ProxyLoad
  = ProxyLoad
      { proxy :: Proxy,
        load :: Int32
      }
  deriving (Show, Eq)

mkProxyLoadByProxy :: Proxy -> ProxyLoad
mkProxyLoadByProxy proxy =
  ProxyLoad
    { load = 0,
      ..
    }
