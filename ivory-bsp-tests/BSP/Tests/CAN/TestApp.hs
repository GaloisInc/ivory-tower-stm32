{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.CAN.TestApp where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import qualified Ivory.BSP.STM32F405.Interrupt as F405

import BSP.Tests.LED
import BSP.Tests.Platforms

app :: (e -> ClockConfig)
    -> (e -> TestCAN F405.Interrupt)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestcan toleds = do
  can <- fmap totestcan getEnv
  leds <- fmap toleds getEnv

  (req, res) <- canTower tocc (testCAN can) 500000 (testCANRX can) (testCANTX can)

  periodic <- period (Milliseconds 250)

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can) [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []
        ledSetup $ redLED leds
        ledOn $ redLED leds

    handler periodic "periodic" $ do
      req_emitter <- emitter req 1
      callbackV $ \ p -> do
        let time :: Uint64
            time = signCast $ toIMicroseconds p
        r <- local $ istruct
          [ tx_id  .= ival 0x7FF
          , tx_ide .= ival false
          , tx_rtr .= ival false
          , tx_buf .= iarray [ ival $ bitCast $ time `iShiftR` fromInteger (8 * i) | i <- [7,6..0] ]
          , tx_len .= ival 8
          ]
        emit req_emitter $ constRef r

    received <- stateInit "can_received_count" (ival (0 :: Uint32))
    handler res "result" $ do
      callback $ const $ do
        count <- deref received
        store received (count + 1)
        ifte_ (count .& 1 ==? 0)
          (ledOff $ redLED leds)
          (ledOn $ redLED leds)
