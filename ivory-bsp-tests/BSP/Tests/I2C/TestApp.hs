{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.I2C.TestApp where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.ClockConfig

import BSP.Tests.Platforms

app :: (e -> ClockConfig)
    -> (e -> TestI2C)
    -> Tower e ()
app tocc totesti2c = do
  i2c <- fmap totesti2c getEnv
  (BackpressureTransmit req res, _ready) <- i2cTower tocc (testI2C i2c) (testI2CPins i2c)

  periodic <- period (Milliseconds 250)
  monitor "simplecontroller" $ do
    handler periodic "periodic" $ do
      req_emitter <- emitter req 1
      callback $ \pref -> do
        p <- deref pref
        ifte_ ((p .% 500000) >=? 250000)
          (do r <- local $ istruct
                     -- Write values 0xF2, 0xF3, 0xEE to page 0
                     [ tx_addr   .= ival eepromaddr
                     , tx_buf    .= iarray [ival 0x00, ival 0xF2, ival 0xF3, ival 0xEE]
                     , tx_len    .= ival 4
                     , rx_len    .= ival 0
                     ]
              emit req_emitter (constRef r))
          (do r <- local $ istruct
                     [ tx_addr   .= ival eepromaddr
                     -- Read 3 values out of page 0 (should be 0xF2, 0xF3, 0xEE)
                     , tx_buf    .= iarray [ival 0x00]
                     , tx_len    .= ival 1
                     , rx_len    .= ival 3
                     ]
              emit req_emitter (constRef r))

    handler res "result" $ do
      callback $ const $ return ()
  where
  -- Test against an AT24 EEPROM or equivalent
  eepromaddr = I2CDeviceAddr 0x50
