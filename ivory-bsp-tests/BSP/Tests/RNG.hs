{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.RNG (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink
import BSP.Tests.UART.Types

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.RNG
import Ivory.BSP.STM32.ClockConfig

--------------------------------------------------------------------------------

rngTestTypes :: Module
rngTestTypes = package "rngTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

app :: (e -> ColoredLEDs)
    -> (e -> ClockConfig)
    -> (e -> TestUART)
    -> (e -> RNG)
    -> Tower e ()
app toleds tocc touart torng = do
  towerModule  rngTestTypes
  towerDepends rngTestTypes

  e <- getEnv
  blink p [blueLED (toleds e)]
  let u = touart e
  (BackpressureTransmit req res, _istream, mon) <- uartTower tocc (testUARTPeriph u) (testUARTPins u) 115200
  monitor "dma" mon
  rand_nums <- rngTower (torng e) (Milliseconds 5)
  randReporter "bsp-rng-test\r\n" req res rand_nums
  where
  p = Milliseconds 333

--------------------------------------------------------------------------------

randReporter :: String
             -> ChanInput  UARTBuffer
             -> ChanOutput ('Stored IBool)
             -> ChanOutput ('Stored Uint32)
             -> Tower p ()
randReporter greeting req res rands = do
  p <- period (Milliseconds 1)

  monitor "randReporter" $ do
    (out_req :: Ref 'Global UARTBuffer) <- state "out_req"

    let puts :: String -> Ivory eff ()
        puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str

        putc :: Uint8 -> Ivory eff ()
        putc byte = do
          pos <- deref (out_req ~> stringLengthL)
          when (pos <? arrayLen (out_req ~> stringDataL)) $ do
            store (out_req ~> stringDataL ! toIx pos) byte
            store (out_req ~> stringLengthL) (pos + 1)

        flush :: (GetAlloc eff ~ 'Scope cs)
              => Emitter UARTBuffer -> Ivory eff ()
        flush e = do
          emit e (constRef out_req)
          store (out_req ~> stringLengthL) 0

    initialized <- stateInit "initialized" (ival false)
    handler p "init" $ do
      e <- emitter req 1
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts (greeting ++ "\r\n")
          flush e

    handler rands "rands" $ do
      e <- emitter req 1
      callbackV $ \r -> do
        vs <- local (iarray [ ival (r `bitsAt` 24)
                            , ival (r `bitsAt` 16)
                            , ival (r `bitsAt` 8)
                            , ival (r `bitsAt` 0)])
        arrayMap $ \(i :: Ix 4) ->
          putc =<< deref (constRef (vs ! i))
        flush e

    handler res "result" $ return () -- XXX

    where
    bitsAt :: Uint32 -> Int -> Uint8
    bitsAt v n = bitCast (v `iShiftR` (fromIntegral n))
