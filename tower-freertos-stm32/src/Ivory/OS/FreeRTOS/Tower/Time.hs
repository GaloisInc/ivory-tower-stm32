{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Tower.Time
  ( time_module
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS.Time as T

time_module :: Module
time_module = package "tower_time" $ do
  T.moddef
  incl getTimeProc
  where
  getTimeProc :: Def('[]':->ITime)
  getTimeProc = proc "tower_get_time" $ body $ do
    ticks  <- call T.getTickCount
    ratems <- call T.getTickRateMilliseconds
    let t_us :: Sint64
        t_us = ((safeCast ticks) * 1000) `iDiv` (safeCast ratems)
    ret (fromIMicroseconds t_us)
