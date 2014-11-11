

module Ivory.OS.FreeRTOS.Tower.STM32.Config
  ( Config(..)
  , Processor(..)
  , Bootloader(..)
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Processor
import Tower.Config

data Config =
  Config
    { config_processor  :: Processor
    , config_bootloader :: Bootloader
    , config_clock      :: ClockConfig
    }

data Bootloader
  = NoBootloader
  | PX4ProjectBootloader

instance Configurable Bootloader where
  fromConfig v = case fromConfig v of
    Just False -> Just NoBootloader
    _ -> case fromConfig v of
      Just "none"                 -> Just NoBootloader
      Just "NoBootloader"         -> Just NoBootloader
      Just "px4"                  -> Just PX4ProjectBootloader
      Just "PX4ProjectBootloader" -> Just PX4ProjectBootloader
      _ -> Nothing

instance Configurable Config where
  fromConfig v = do
    body <- element "stm32" v
    p <- fromConfig =<< element "processor" body
    b <- fromConfig =<< element "bootloader" body
    c <- fromConfig body
    return Config
      { config_processor = p
      , config_bootloader = b
      , config_clock = c
      }

