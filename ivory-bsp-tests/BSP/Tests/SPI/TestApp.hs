{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.SPI.TestApp where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import           Ivory.BSP.STM32.Peripheral.SPI
import           Ivory.BSP.STM32.Driver.SPI
import           Ivory.BSP.STM32.ClockConfig

import           Ivory.BSP.STM32F405.GPIO -- XXX FIXME

import BSP.Tests.Platforms

app :: (e -> ClockConfig)
    -> (e -> TestSPI)
    -> Tower e ()
app tocc tospi = do
  testspi <- fmap tospi getEnv
  (BackpressureTransmit req res, _ready) <- spiTower tocc
                          [ testdevice1 testspi, testdevice2 testspi ]
                          (testSPIPins testspi)
  per <- period (Milliseconds 250)
  monitor "simplecontroller" $ do
    handler per "periodic" $ do
      req_emitter <- emitter req 1
      callback $ \tref -> do
        t <- deref tref
        ifte_ ((t .% 500000) >=? 250000)
          (do r <- local $ istruct
                     [ tx_device .= ival (SPIDeviceHandle 0) -- Should be pinE2 / 1mhz
                     , tx_buf    .= iarray [ival 0xF1, ival 0xF2, ival 0xF3]
                     , tx_len    .= ival 3
                     ]
              emit req_emitter (constRef r))
          (do r <- local $ istruct
                     [ tx_device .= ival (SPIDeviceHandle 1) -- Should be pinE3 / 500khz
                     , tx_buf    .= iarray [ival 0xF4, ival 0xF5, ival 0xF6, ival 0xF7]
                     , tx_len    .= ival 4
                     ]
              emit req_emitter (constRef r))

    handler res "result" $ callback $ \r -> do
      code <- deref (r ~> resultcode)
      len  <- deref (r ~> rx_idx)
      assert (code ==? 0)
      assert ((len  ==? 3) .|| (len ==? 4))

  where
  -- XXX MOVE THESE TEST DEVICES INTO THE PLATFORM, SO THEY CAN BE DIFFERENT
  -- FOR THE PIXHAWK ETC
  testdevice1 testspi = SPIDevice
    { spiDevPeripheral    = testSPIPeriph testspi
    , spiDevCSPin         = pinE2
    , spiDevClockHz       = 250000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "testdevice1_250khz_pinE2"
    }
  testdevice2 testspi = SPIDevice
    { spiDevPeripheral    = testSPIPeriph testspi
    , spiDevCSPin         = pinE3
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "testdevice2_500khz_pinE3"
    }

