{-# LANGUAGE RecordWildCards #-}

module Types.Domain.ProxyStatus where

import qualified Control.Concurrent.Chan as Chan
import qualified Data.Time as Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Types.Domain.Proxy (Proxy)
import qualified Types.Domain.ProxyLoad as ProxyLoad

type Minute = Int

data ProxyParams
  = ProxyParams
      { proxyLoad :: ProxyLoad.ProxyLoad,
        countTry :: Minute,
        time :: Time.UTCTime
      }
  deriving (Show)

getProxy :: ProxyParams -> Proxy
getProxy pp = ProxyLoad.proxy $ proxyLoad pp

type ProxyStatus = Chan.Chan ProxyParams

initProxyStatus :: IO ProxyStatus
initProxyStatus = Chan.newChan

getProxyLoad :: ProxyStatus -> IO (Either Minute ProxyParams)
getProxyLoad ps = do
  pp@ProxyParams {..} <- Chan.readChan ps
  curTime <- Time.getCurrentTime
  let diffTime = Time.diffUTCTime curTime time - fiveMinute
  print diffTime
  pure $
    if diffTime >= 0
      then Right pp
      else Left $ round (diffTime / 60) * (-1)

addProxyLoad :: ProxyParams -> ProxyStatus -> IO ()
addProxyLoad pp ps = do
  if countTry pp >= 10
    then do
      time <- Time.getCurrentTime
      print $ "gen new time: " <> show time
      let newCountTry = 0
      let newPp = pp {countTry = newCountTry, time = time}
      Chan.writeChan ps newPp
    else do
      let newPp = pp {countTry = countTry pp + 1}
      Chan.writeChan ps newPp

addProxyLoads :: [ProxyLoad.ProxyLoad] -> ProxyStatus -> IO ()
addProxyLoads proxyLoads ps = do
  let nullDay = fromOrdinalDate 0 0
  let nullTime = Time.UTCTime nullDay 0
  Chan.writeList2Chan ps $ map (\pl -> ProxyParams pl 0 nullTime) proxyLoads

fiveMinute :: Time.NominalDiffTime
fiveMinute = 5 * 60
