{-# LANGUAGE RecordWildCards #-}

module Types.Domain.ProxyStatus where

import qualified Control.Concurrent.Chan as Chan
import qualified Data.Time as Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Types.Domain.ProxyLoad (ProxyLoad)

data ProxyParams
  = ProxyParams
      { proxyLoad :: ProxyLoad,
        countTry :: Int,
        time :: Time.UTCTime
      }
  deriving (Show)

type ProxyStatus = Chan.Chan ProxyParams

initProxyStatus :: IO ProxyStatus
initProxyStatus = Chan.newChan

getProxyLoad :: ProxyStatus -> IO (Either Int (ProxyLoad, Int))
getProxyLoad proxyStatus = do
  ProxyParams {..} <- Chan.readChan proxyStatus
  curTime <- Time.getCurrentTime
  let diffTime = Time.diffUTCTime curTime time - fiveMinute
  pure $
    if diffTime < 0
      then Right (proxyLoad, countTry)
      else Left $ round (diffTime / 60)

addProxyLoad :: ProxyLoad -> Int -> ProxyStatus -> IO ()
addProxyLoad proxyLoad countTry ps = do
  time <- Time.getCurrentTime
  Chan.writeChan ps ProxyParams {..}

addProxyLoads :: [ProxyLoad] -> ProxyStatus -> IO ()
addProxyLoads proxyLoads ps = do
  let nullDay = fromOrdinalDate 0 0
  let nullTime = Time.UTCTime nullDay 0
  Chan.writeList2Chan ps $ map (\pl -> ProxyParams pl 0 nullTime) proxyLoads

fiveMinute :: Time.NominalDiffTime
fiveMinute = 5 * 60
