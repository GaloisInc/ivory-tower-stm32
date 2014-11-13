

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( STM32Config(..)
  , Processor(..)
  , Bootloader(..)
  , stm32f405Defaults
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Processor
import Tower.Config

data STM32Config =
  STM32Config
    { stm32config_processor  :: Processor
    , stm32config_bootloader :: Bootloader
    , stm32config_clock      :: ClockConfig
    }

data Bootloader
  = NoBootloader
  | PX4ProjectBootloader

stm32f405Defaults :: Integer -> STM32Config
stm32f405Defaults xtal_mhz = STM32Config
  { stm32config_processor = STM32F405
  , stm32config_bootloader = NoBootloader
  , stm32config_clock = externalXtal xtal_mhz 168
  }

instance Configurable Bootloader where
  fromConfig v = case fromConfig v of
    Just False -> Just NoBootloader
    _ -> case fromConfig v of
      Just "none"                 -> Just NoBootloader
      Just "NoBootloader"         -> Just NoBootloader
      Just "px4"                  -> Just PX4ProjectBootloader
      Just "PX4ProjectBootloader" -> Just PX4ProjectBootloader
      _ -> Nothing

instance Configurable STM32Config where
  fromConfig v = do
    body <- element "stm32" v
    p <- fromConfig =<< element "processor" body
    b <- fromConfig =<< element "bootloader" body
    c <- fromConfig body
    return STM32Config
      { stm32config_processor = p
      , stm32config_bootloader = b
      , stm32config_clock = c
      }

