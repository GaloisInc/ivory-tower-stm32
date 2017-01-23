{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
--
-- GPIO.hs --- GPIO Peripheral driver.
-- Defines peripheral types, instances, and public API.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes
import Ivory.BSP.STM32.Peripheral.GPIOF4.Regs

-- | A GPIO port, defined as the set of registers that operate on all
-- the pins for that port.
data GPIOPort = GPIOPort
  { gpioPortMODER       :: BitDataReg GPIO_MODER
  , gpioPortOTYPER      :: BitDataReg GPIO_OTYPER
  , gpioPortOSPEEDR     :: BitDataReg GPIO_OSPEEDR
  , gpioPortPUPDR       :: BitDataReg GPIO_PUPDR
  , gpioPortIDR         :: BitDataReg GPIO_IDR
  , gpioPortBSRR        :: BitDataReg GPIO_BSRR
  , gpioPortAFRL        :: BitDataReg GPIO_AFRL
  , gpioPortAFRH        :: BitDataReg GPIO_AFRH
  , gpioPortRCCEnable   :: forall eff . Ivory eff ()
  , gpioPortRCCDisable  :: forall eff . Ivory eff ()
  , gpioPortNumber      :: Int
  , gpioPortName        :: String
  }

-- | Create a GPIO port given the base register address.
mkGPIOPort :: Integer
           -> (forall eff . Ivory eff ())
           -> (forall eff . Ivory eff ())
           -> Int
           -> GPIOPort
mkGPIOPort base rccen rccdis idx =
  GPIOPort
    { gpioPortMODER          = reg 0x00 "mode"
    , gpioPortOTYPER         = reg 0x04 "otype"
    , gpioPortOSPEEDR        = reg 0x08 "ospeed"
    , gpioPortPUPDR          = reg 0x0C "pupd"
    , gpioPortIDR            = reg 0x10 "idr"
    , gpioPortBSRR           = reg 0x18 "bsrr"
    , gpioPortAFRL           = reg 0x20 "afrl"
    , gpioPortAFRH           = reg 0x24 "afrh"
    , gpioPortRCCEnable      = rccen
    , gpioPortRCCDisable     = rccdis
    , gpioPortNumber         = idx
    , gpioPortName           = n
    }
  where
  n = "gpio" ++ [toEnum (fromEnum 'A' + idx)]
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

-- | A GPIO alternate function register and bit field.
data GPIOPinAFR = AFRL (BitDataField GPIO_AFRL GPIO_AF)
                | AFRH (BitDataField GPIO_AFRH GPIO_AF)

-- | A GPIO pin, defined as the accessor functions to manipulate the
-- bits in the registers for the port the pin belongs to.
data GPIOPin = GPIOPin
  { gpioPinPort         :: GPIOPort
  , gpioPinNumber       :: Int
  , gpioPinMode_F       :: BitDataField GPIO_MODER GPIO_Mode
  , gpioPinOutputType_F :: BitDataField GPIO_OTYPER GPIO_OutputType
  , gpioPinSpeed_F      :: BitDataField GPIO_OSPEEDR GPIO_Speed
  , gpioPinPUPD_F       :: BitDataField GPIO_PUPDR GPIO_PUPD
  , gpioPinIDR_F        :: BitDataField GPIO_IDR Bit
  , gpioPinSetBSRR_F    :: BitDataField GPIO_BSRR Bit
  , gpioPinClearBSRR_F  :: BitDataField GPIO_BSRR Bit
  , gpioPinAFR_F        :: GPIOPinAFR
  }

pinName :: GPIOPin -> String
pinName p = gpioPortName (gpioPinPort p) ++ show (gpioPinNumber p)

-- | Enable the GPIO port for a pin in the RCC.
pinEnable :: GPIOPin -> Ivory eff ()
pinEnable = gpioPortRCCEnable . gpioPinPort

pinDisable :: GPIOPin -> Ivory eff ()
pinDisable = gpioPortRCCDisable . gpioPinPort

-- | Set a GPIO to a default floating input state
pinUnconfigure :: GPIOPin -> Ivory eff ()
pinUnconfigure p = do
  pinDisable p
  pinSetMode p gpio_mode_input
  pinSetPUPD p gpio_pupd_none

setRegF :: (BitData a, BitData b, IvoryIOReg (BitDataRep a),
            SafeCast (BitDataRep b) (BitDataRep a))
        => (GPIOPort -> BitDataReg a)
        -> (GPIOPin  -> BitDataField a b)
        -> GPIOPin
        -> b
        -> Ivory eff ()
setRegF reg field pin val =
  modifyReg (reg $ gpioPinPort pin) $
    setField (field pin) val

pinSetMode :: GPIOPin -> GPIO_Mode -> Ivory eff ()
pinSetMode = setRegF gpioPortMODER gpioPinMode_F

pinSetOutputType :: GPIOPin -> GPIO_OutputType -> Ivory eff ()
pinSetOutputType = setRegF gpioPortOTYPER gpioPinOutputType_F

pinSetSpeed :: GPIOPin -> GPIO_Speed -> Ivory eff ()
pinSetSpeed = setRegF gpioPortOSPEEDR gpioPinSpeed_F

pinSetPUPD :: GPIOPin -> GPIO_PUPD -> Ivory eff ()
pinSetPUPD = setRegF gpioPortPUPDR gpioPinPUPD_F

pinSetAF :: GPIOPin -> GPIO_AF -> Ivory eff ()
pinSetAF pin af =
  case gpioPinAFR_F pin of
    AFRL field -> setRegF gpioPortAFRL (const field) pin af
    AFRH field -> setRegF gpioPortAFRH (const field) pin af

pinSet :: GPIOPin -> Ivory eff ()
pinSet pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $
    setBit (gpioPinSetBSRR_F pin)

pinClear :: GPIOPin -> Ivory eff ()
pinClear pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $
    setBit (gpioPinClearBSRR_F pin)

pinRead :: GPIOPin -> Ivory eff IBool
pinRead pin = do
  r <- getReg (gpioPortIDR $ gpioPinPort pin)
  return (bitToBool (r #. gpioPinIDR_F pin))

