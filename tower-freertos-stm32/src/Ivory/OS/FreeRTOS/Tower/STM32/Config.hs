

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( Config(..)
  , Processor(..)
  , Bootloader(..)
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Processor

data Config =
  Config
    { config_processor  :: Processor
    , config_bootloader :: Bootloader
    , config_clock      :: ClockConfig
    }

data Bootloader
  = NoBootloader
  | PX4ProjectBootloader

