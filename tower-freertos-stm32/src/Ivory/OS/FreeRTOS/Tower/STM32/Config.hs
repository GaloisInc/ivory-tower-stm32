

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( STM32Config(..)
  , Processor(..)
  , Bootloader(..)
  , stm32f405Defaults
  , bootloaderParser
  , stm32ConfigParser
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


bootloaderParser :: ConfigParser Bootloader
bootloaderParser = string >>= \s ->
  case s of
   "none"                 -> return NoBootloader
   "NoBootloader"         -> return NoBootloader
   "px4"                  -> return PX4ProjectBootloader
   "PX4ProjectBootloader" -> return PX4ProjectBootloader
   _ -> fail ("expected Bootloader, got " ++ s)

stm32ConfigParser :: ConfigParser STM32Config
stm32ConfigParser = subsection "stm32" $ do
  p <- subsection "processor" (processorParser `withDefault` STM32F405)
  b <- subsection "bootloader" (bootloaderParser `withDefault` NoBootloader)
  c <- clockConfigParser
  return STM32Config
    { stm32config_processor = p
    , stm32config_bootloader = b
    , stm32config_clock = c
    }

