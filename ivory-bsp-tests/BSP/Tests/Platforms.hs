{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.Platforms
  ( testPlatformParser
  , ColoredLEDs(..)
  , TestUART(..)
  , TestSPI(..)
  , TestI2C(..)
  , TestCAN(..)
  , TestPlatform(..)
  , px4fmuv17
  , f4discovery
  , open407vc
  ) where

import Tower.Config
import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.OS.FreeRTOS.Tower.STM32.Config

import BSP.Tests.LED

testPlatformParser :: ConfigParser (TestPlatform F405.Interrupt)
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17"   -> return px4fmuv17
    "F4DISCOVERY" -> return f4discovery
    "OPEN407VC"   -> return open407vc
    "PORT407Z"    -> return port407z
    _ -> fail ("no such platform " ++ p)

data ColoredLEDs =
  ColoredLEDs
    { redLED  :: LED
    , blueLED :: LED
    }

data TestUART s =
  TestUART
    { testUART :: UART s
    }

data TestSPI s =
  TestSPI
    { testSPI :: SPIPeriph s
    }

data TestI2C s =
  TestI2C
    { testI2C :: I2CPeriph s
    , testSDA :: GPIOPin
    , testSCL :: GPIOPin
    }

data TestCAN s =
  TestCAN
    { testCAN        :: CANPeriph s
    , testCANRX      :: GPIOPin
    , testCANTX      :: GPIOPin
    , testCANFilters :: CANPeriphFilters
    }

data TestPlatform s =
  TestPlatform
    { testplatform_leds  :: ColoredLEDs
    , testplatform_uart  :: TestUART s
    , testplatform_spi   :: TestSPI s
    , testplatform_i2c   :: TestI2C s
    , testplatform_can   :: TestCAN s
    , testplatform_stm32 :: STM32Config
    }

---------- PX4FMUv17 ----------------------------------------------------------

px4fmuv17 :: TestPlatform F405.Interrupt
px4fmuv17 = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinB14 ActiveLow
      , blueLED = LED F405.pinB15 ActiveLow
      }
  , testplatform_uart = TestUART
      { testUART = F405.uart5
      }
  , testplatform_spi = TestSPI
      { testSPI = F405.spi3
      }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c1
      , testSDA = F405.pinB6
      , testSCL = F405.pinB7
      }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinD0
      , testCANTX = F405.pinD1
      , testCANFilters = F405.canFilters
      }
  , testplatform_stm32 = stm32f405Defaults 24
  }


---------- F4Discovery --------------------------------------------------------

f4discovery :: TestPlatform F405.Interrupt
f4discovery = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinD14 ActiveHigh
      , blueLED = LED F405.pinD15 ActiveHigh
      }
  , testplatform_uart = TestUART
      { testUART = F405.uart1
      }
  , testplatform_spi = TestSPI
      { testSPI = F405.spi3
      }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c1
      , testSDA = F405.pinB6
      , testSCL = F405.pinB7
      }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinD0
      , testCANTX = F405.pinD1
      , testCANFilters = F405.canFilters
      }
  , testplatform_stm32 = stm32f405Defaults 8
  }

---------- Open407VC ----------------------------------------------------------

open407vc :: TestPlatform F405.Interrupt
open407vc = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinD12 ActiveHigh
      , blueLED = LED F405.pinD13 ActiveHigh
      }
  , testplatform_uart = TestUART
      { testUART = F405.uart2
      }
  , testplatform_spi = TestSPI
      { testSPI = F405.spi3
      }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c1
      , testSDA = F405.pinB6
      , testSCL = F405.pinB7
      }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinD0
      , testCANTX = F405.pinD1
      , testCANFilters = F405.canFilters
      }
  , testplatform_stm32 = stm32f405Defaults 8
  }


---------- Port407Z -----------------------------------------------------------

port407z :: TestPlatform F405.Interrupt
port407z = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinA4 ActiveHigh -- LED1
      , blueLED = LED F405.pinA5 ActiveHigh -- LED2
      }
  , testplatform_uart = TestUART
      { testUART = F405.uart2
      }
  , testplatform_spi = TestSPI
      { testSPI = F405.spi3
      }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c1
      , testSDA = F405.pinB6
      , testSCL = F405.pinB7
      }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinD0
      , testCANTX = F405.pinD1
      , testCANFilters = F405.canFilters
      }
  , testplatform_stm32 = stm32f405Defaults 8
  }


