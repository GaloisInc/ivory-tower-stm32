{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
--
-- Peripheral.hs --- SPI peripheral driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.SPI.Peripheral where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.SPI.RegTypes
import Ivory.BSP.STM32.Peripheral.SPI.Regs

import Ivory.BSP.STM32.Peripheral.GPIOF4

data SPIPeriph = SPIPeriph
  { spiRegCR1      :: BitDataReg SPI_CR1
  , spiRegCR2      :: BitDataReg SPI_CR2
  , spiRegSR       :: BitDataReg SPI_SR
  , spiRegDR       :: BitDataReg SPI_DR
  , spiRCCEnable   :: forall eff . Ivory eff ()
  , spiRCCDisable  :: forall eff . Ivory eff ()
  , spiInterrupt   :: HasSTM32Interrupt
  , spiPClk        :: PClk
  , spiName        :: String
  }

data SPIPins = SPIPins
  { spiPinMiso     :: GPIOPin
  , spiPinMosi     :: GPIOPin
  , spiPinSck      :: GPIOPin
  , spiPinAF       :: GPIO_AF
  }

mkSPIPeriph :: (STM32Interrupt i)
            => Integer
            -> (forall eff . Ivory eff ())
            -> (forall eff . Ivory eff ())
            -> i
            -> PClk
            -> String
            -> SPIPeriph
mkSPIPeriph base rccen rccdis inter pclk n =
  SPIPeriph
    { spiRegCR1      = reg 0x00 "cr1"
    , spiRegCR2      = reg 0x04 "cr2"
    , spiRegSR       = reg 0x08 "sr"
    , spiRegDR       = reg 0x0C "dr"
    , spiRCCEnable   = rccen
    , spiRCCDisable  = rccdis
    , spiInterrupt   = HasSTM32Interrupt inter
    , spiPClk        = pclk
    , spiName        = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)


initInPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initInPin pin af = do
  comment ("init spi input pin " ++ pinName pin)
  pinEnable  pin
  pinSetAF   pin af
  pinSetMode pin gpio_mode_af
  pinSetPUPD pin gpio_pupd_none
  pinSetSpeed pin gpio_speed_50mhz

initOutPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initOutPin pin af = do
  comment ("init spi output pin " ++ pinName pin)
  pinEnable        pin
  pinSetAF         pin af
  pinSetMode       pin gpio_mode_af
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed      pin gpio_speed_50mhz

-- | Enable peripheral and setup GPIOs. Must be performed
--   before any other SPI peripheral actions.
spiInit :: (GetAlloc eff ~ 'Scope s) => SPIPeriph -> SPIPins -> Ivory eff ()
spiInit spi pins = do
  spiRCCEnable spi
  spiClearCr1 spi
  spiClearCr2 spi
  initInPin  (spiPinMiso pins) (spiPinAF pins)
  initOutPin (spiPinMosi pins) (spiPinAF pins)
  initOutPin (spiPinSck  pins) (spiPinAF pins)

spiInitISR :: (GetAlloc eff ~ 'Scope s)
           => SPIPeriph -> Ivory eff ()
spiInitISR spi = interrupt_enable $ spiInterrupt spi

-- Clock Polarity and Phase: see description
-- of CPOL and CPHA in ST reference manual RM0090
data SPICSActive      = ActiveHigh | ActiveLow
data SPIClockPolarity = ClockPolarityLow | ClockPolarityHigh
data SPIClockPhase    = ClockPhase1 | ClockPhase2
data SPIBitOrder      = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevPeripheral    :: SPIPeriph
  , spiDevCSPin         :: GPIOPin
  , spiDevClockHz       :: Integer
  , spiDevCSActive      :: SPICSActive
  , spiDevClockPolarity :: SPIClockPolarity
  , spiDevClockPhase    :: SPIClockPhase
  , spiDevBitOrder      :: SPIBitOrder
  , spiDevName          :: String
  }

spiDeviceInit :: (GetAlloc eff ~ 'Scope s) => SPIDevice -> Ivory eff ()
spiDeviceInit dev = do
  let pin = spiDevCSPin dev
  pinEnable         pin
  spiDeviceDeselect dev
  pinSetMode        pin gpio_mode_output
  pinSetOutputType  pin gpio_outputtype_pushpull
  pinSetSpeed       pin gpio_speed_2mhz

spiBusBegin :: (GetAlloc eff ~ 'Scope cs)
            => ClockConfig -> SPIDevice -> Ivory eff ()
spiBusBegin clockconfig dev = do
  modifyReg (spiRegCR1 periph) $ clearBit spi_cr1_spe

  let baud = spiDevBaud clockconfig periph (spiDevClockHz dev)
  modifyReg (spiRegCR1 periph) $ do
    setBit   spi_cr1_mstr
    setBit   spi_cr1_ssm
    setBit   spi_cr1_ssi
    setField spi_cr1_br baud
    case spiDevClockPolarity dev of
      ClockPolarityLow  -> clearBit spi_cr1_cpol
      ClockPolarityHigh -> setBit  spi_cr1_cpol
    case spiDevClockPhase dev of
      ClockPhase1 -> clearBit spi_cr1_cpha
      ClockPhase2 -> setBit   spi_cr1_cpha
    case spiDevBitOrder dev of
      LSBFirst -> setBit   spi_cr1_lsbfirst
      MSBFirst -> clearBit spi_cr1_lsbfirst

  modifyReg (spiRegCR1 periph) $ setBit   spi_cr1_spe
  where
  periph = spiDevPeripheral dev


spiBusEnd :: SPIPeriph -> Ivory eff ()
spiBusEnd  periph =
  spiModifyCr1 periph [ spi_cr1_spe ] false

spiDeviceEnd :: SPIDevice -> Ivory eff ()
spiDeviceEnd dev = do
  spiDeviceDeselect dev
  spiBusEnd         periph
  where periph = spiDevPeripheral dev


spiSetTXEIE :: SPIPeriph -> Ivory eff ()
spiSetTXEIE spi = modifyReg (spiRegCR2 spi) $ setBit spi_cr2_txeie

spiClearTXEIE :: SPIPeriph -> Ivory eff ()
spiClearTXEIE spi = modifyReg (spiRegCR2 spi) $ clearBit spi_cr2_txeie

spiSetRXNEIE :: SPIPeriph -> Ivory eff ()
spiSetRXNEIE spi = modifyReg (spiRegCR2 spi) $ setBit spi_cr2_rxneie

spiClearRXNEIE :: SPIPeriph -> Ivory eff ()
spiClearRXNEIE spi = modifyReg (spiRegCR2 spi) $ clearBit spi_cr2_rxneie

spiGetDR :: SPIPeriph -> Ivory eff Uint8
spiGetDR spi = do
  r <- getReg (spiRegDR spi)
  return (toRep (r #. spi_dr_data))

spiSetDR :: SPIPeriph -> Uint8 -> Ivory eff ()
spiSetDR spi b =
  setReg (spiRegDR spi) $
    setField spi_dr_data (fromRep b)

-- Internal Helper Functions ---------------------------------------------------

spiDevBaud :: ClockConfig -> SPIPeriph -> Integer -> SPIBaud
spiDevBaud clockconfig periph target = foldl aux spi_baud_div_256 tbl
  where
  fpclk = clockPClkHz (spiPClk periph) clockconfig
  aux dflt (divider, spibaud) = if fpclk `div` divider <= target then spibaud else dflt
  tbl = [( 256, spi_baud_div_256 )
        ,( 128, spi_baud_div_128 )
        ,( 64,  spi_baud_div_64 )
        ,( 32,  spi_baud_div_32 )
        ,( 16,  spi_baud_div_16 )
        ,( 8,   spi_baud_div_8 )
        ,( 4,   spi_baud_div_4 )
        ,( 2,   spi_baud_div_2 )]

spiDeviceSelect   :: SPIDevice -> Ivory eff ()
spiDeviceSelect dev = case spiDevCSActive dev of
  ActiveHigh -> pinSet   (spiDevCSPin dev)
  ActiveLow  -> pinClear (spiDevCSPin dev)

spiDeviceDeselect :: SPIDevice -> Ivory eff ()
spiDeviceDeselect dev = case spiDevCSActive dev of
  ActiveHigh -> pinClear (spiDevCSPin dev)
  ActiveLow  -> pinSet   (spiDevCSPin dev)

spiSetBaud :: SPIPeriph -> SPIBaud -> Ivory eff ()
spiSetBaud periph baud = modifyReg (spiRegCR1 periph) $ setField spi_cr1_br baud

spiModifyCr1 :: SPIPeriph -> [BitDataField SPI_CR1 Bit] -> IBool -> Ivory eff ()
spiModifyCr1 periph fields b =
  modifyReg (spiRegCR1 periph) $ mapM_ (\f -> setField f (boolToBit b)) fields

spiClearCr1 :: SPIPeriph -> Ivory eff ()
spiClearCr1 periph = modifyReg (spiRegCR1 periph) $ do
  -- It may not be strictly necessary to clear all of these fields.
  -- I'm copying the implementation of the HWF4 lib, where REG->CR1 is set to 0
  clearBit spi_cr1_bidimode
  clearBit spi_cr1_bidioe
  clearBit spi_cr1_crcen
  clearBit spi_cr1_crcnext
  clearBit spi_cr1_dff
  clearBit spi_cr1_rxonly
  clearBit spi_cr1_ssm
  clearBit spi_cr1_ssi
  clearBit spi_cr1_lsbfirst
  clearBit spi_cr1_spe
  setField spi_cr1_br spi_baud_div_2
  clearBit spi_cr1_mstr
  clearBit spi_cr1_cpol
  clearBit spi_cr1_cpha

spiClearCr2 :: SPIPeriph -> Ivory eff ()
spiClearCr2 periph = modifyReg (spiRegCR2 periph) $ do
  -- May not be strictly necessary to set all these fields, see comment
  -- for spiClearCr1
  clearBit spi_cr2_txeie
  clearBit spi_cr2_rxneie
  clearBit spi_cr2_errie
  clearBit spi_cr2_ssoe
  clearBit spi_cr2_txdmaen
  clearBit spi_cr2_rxdmaen

spiSetClockPolarity :: SPIPeriph -> SPIClockPolarity -> Ivory eff ()
spiSetClockPolarity periph polarity =
  modifyReg (spiRegCR1 periph) $ case polarity of
    ClockPolarityLow  -> clearBit spi_cr1_cpol
    ClockPolarityHigh -> setBit  spi_cr1_cpol

spiSetClockPhase :: SPIPeriph -> SPIClockPhase -> Ivory eff ()
spiSetClockPhase periph phase =
  modifyReg (spiRegCR1 periph) $ case phase of
    ClockPhase1 -> clearBit spi_cr1_cpha
    ClockPhase2 -> setBit   spi_cr1_cpha

spiSetBitOrder :: SPIPeriph -> SPIBitOrder -> Ivory eff ()
spiSetBitOrder periph bitorder =
  modifyReg (spiRegCR1 periph) $ case bitorder of
    LSBFirst -> setBit   spi_cr1_lsbfirst
    MSBFirst -> clearBit spi_cr1_lsbfirst

