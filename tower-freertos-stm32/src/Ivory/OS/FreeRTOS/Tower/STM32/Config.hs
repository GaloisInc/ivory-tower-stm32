

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( Config(..)
  , Processor(..)
  , Bootloader(..)
  , defaultConfig
  ) where

import Ivory.BSP.STM32.Processor

data Config =
  Config
    { config_processor :: Processor
    , config_bootloader :: Bootloader
    }

data Bootloader
  = NoBootloader
  | PX4ProjectBootloader

defaultConfig :: Config
defaultConfig = Config
  { config_processor = STM32F405
  , config_bootloader = NoBootloader
  }

