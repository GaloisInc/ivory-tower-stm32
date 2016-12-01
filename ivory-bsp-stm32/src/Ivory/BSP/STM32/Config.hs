

module Ivory.BSP.STM32.Config
  ( STM32Config(..)
  , Processor(..)
  , PX4Version(..)
  , stm32f405Defaults
  , stm32f427Defaults
  , px4versionParser
  , stm32ConfigParser
  ) where

import qualified Data.Char as C

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Processor
import Ivory.Tower.Config

data STM32Config =
  STM32Config
    { stm32config_processor  :: Processor
    , stm32config_px4version :: Maybe PX4Version
    , stm32config_clock      :: ClockConfig
    , stm32config_sram       :: Integer
    -- ^ sram size in bytes

--    , stm32config_systick    :: SysTickConfig
    }

data PX4Version
  = PX4FMU_v1
  | PX4FMU_v2
  deriving (Eq, Show)

stm32f405Defaults :: Integer -> STM32Config
stm32f405Defaults xtal_mhz = STM32Config
  { stm32config_processor  = STM32F405
  , stm32config_px4version = Nothing
  , stm32config_clock      = externalXtal xtal_mhz 168
  , stm32config_sram       = 128 * 1024
  }

stm32f427Defaults :: Integer -> STM32Config
stm32f427Defaults xtal_mhz = STM32Config
  { stm32config_processor  = STM32F427
  , stm32config_px4version = Nothing
  , stm32config_clock      = externalXtal xtal_mhz 168
  , stm32config_sram       = 192 * 1024
  }

px4versionParser :: ConfigParser (Maybe PX4Version)
px4versionParser = string >>= \s ->
  case map C.toLower s of
   "px4fmuv1"             -> return (Just PX4FMU_v1)
   "px4fmuv2"             -> return (Just PX4FMU_v2)
   "pixhawk"              -> return (Just PX4FMU_v2)
   "none"                 -> return Nothing
   _ -> fail ("expected px4 version, got " ++ s)

stm32ConfigParser :: STM32Config -> ConfigParser STM32Config
stm32ConfigParser conf = (subsection "stm32" stm32parser) `withDefault` conf
  where 
  stm32parser = do
    p <- (subsection "processor" processorParser) `withDefault` (stm32config_processor conf)
    v <- (subsection "px4version" px4versionParser) `withDefault` (stm32config_px4version conf)
    c <- clockConfigParser `withDefault` (stm32config_clock conf)
    s <- (subsection "sram" integer) `withDefault` (stm32config_sram conf)
    return STM32Config
      { stm32config_processor = p
      , stm32config_px4version = v
      , stm32config_clock = c
      , stm32config_sram = s
      }

