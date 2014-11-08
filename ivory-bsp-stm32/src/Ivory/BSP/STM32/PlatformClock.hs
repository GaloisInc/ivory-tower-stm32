
module Ivory.BSP.STM32.PlatformClock where

import Ivory.Tower
import Ivory.BSP.STM32.ClockConfig

-- XXX rename this to HasClockConfig later
class PlatformClock e where
  getClockConfig :: Tower e ClockConfig
