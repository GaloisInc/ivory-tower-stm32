
module Ivory.BSP.STM32F405.ClockConfig
  ( f405ExtXtalMHz
  ) where

import Ivory.BSP.STM32.ClockConfig

-- XXX kill this off
-- Give a clock config for a given external crystal frequency in MHz
f405ExtXtalMHz :: Integer -> ClockConfig
f405ExtXtalMHz xtalfreq = externalXtal xtalfreq 168
