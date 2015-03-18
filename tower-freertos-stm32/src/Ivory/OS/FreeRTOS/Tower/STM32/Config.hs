

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( STM32Config(..)
  , Processor(..)
  , Bootloader(..)
  , PX4Version(..)
  , stm32f405Defaults
  , stm32f427Defaults
  , bootloaderParser
  , stm32ConfigParser
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Processor
import Ivory.Tower.Config

data STM32Config =
  STM32Config
    { stm32config_processor  :: Processor
    , stm32config_bootloader :: Bootloader
    , stm32config_clock      :: ClockConfig
    }

data Bootloader
  = NoBootloader
  | PX4ProjectBootloader PX4Version
  deriving (Eq, Show)

data PX4Version
  = PX4FMU_v1
  | PX4FMU_v2
  deriving (Eq, Show)

stm32f405Defaults :: Integer -> STM32Config
stm32f405Defaults xtal_mhz = STM32Config
  { stm32config_processor = STM32F405
  , stm32config_bootloader = NoBootloader
  , stm32config_clock = externalXtal xtal_mhz 168
  }

stm32f427Defaults :: Integer -> STM32Config
stm32f427Defaults xtal_mhz = STM32Config
  { stm32config_processor = STM32F427
  , stm32config_bootloader = NoBootloader
  , stm32config_clock = externalXtal xtal_mhz 168
  }

bootloaderParser :: Processor -> ConfigParser Bootloader
bootloaderParser p = string >>= \s ->
  case s of
   "none"                 -> return NoBootloader
   "NoBootloader"         -> return NoBootloader
   "px4"                  -> return (PX4ProjectBootloader px4vers)
   "PX4ProjectBootloader" -> return (PX4ProjectBootloader px4vers)
   _ -> fail ("expected Bootloader, got " ++ s)
  where
  px4vers = case p of
    STM32F405 -> PX4FMU_v1
    STM32F427 -> PX4FMU_v2

stm32ConfigParser :: STM32Config -> ConfigParser STM32Config
stm32ConfigParser conf = (subsection "stm32" stm32parser) `withDefault` conf
  where 
  stm32parser = do
    p <- (subsection "processor" processorParser) `withDefault` (stm32config_processor conf)
    b <- (subsection "bootloader" (bootloaderParser p)) `withDefault` (stm32config_bootloader conf)
    c <- clockConfigParser `withDefault` (stm32config_clock conf)
    return STM32Config
      { stm32config_processor = p
      , stm32config_bootloader = b
      , stm32config_clock = c
      }

