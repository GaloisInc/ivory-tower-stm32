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

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.RNG
import Ivory.BSP.STM32.ClockConfig

--------------------------------------------------------------------------------

app :: (e -> ColoredLEDs)
    -> (e -> ClockConfig)
    -> (e -> TestUART)
    -> (e -> RNG)
    -> Tower e ()
app toleds tocc touart torng = do
  e <- getEnv
  blink p [blueLED (toleds e)]
  let u = touart e
  (_istream, ostream) <- uartTower tocc (testUARTPeriph u) (testUARTPins u)
                                  115200 (Proxy :: Proxy 256)
  rand_nums <- rngTower (torng e) (Milliseconds 5)
  randReporter "bsp-rng-test\n\n\n" ostream rand_nums
  where
  p = Milliseconds 333

--------------------------------------------------------------------------------

randReporter :: String
             -> ChanInput  (Stored Uint8)
             -> ChanOutput (Stored Uint32)
             -> Tower p ()
randReporter greeting ostream rands = do
  p <- period (Milliseconds 1)

  let puts :: (GetAlloc eff ~ Scope cs)
           => Emitter (Stored Uint8) -> String -> Ivory eff ()
      puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ Scope cs)
           => Emitter (Stored Uint8) -> Uint8 -> Ivory eff ()
      putc = emitV

  monitor "randReporter" $ do
    initialized <- stateInit "initialized" (ival false)
    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o greeting

    handler rands "rands" $ do
      e <- emitter ostream 4
      callbackV $ \r -> do
        vs <- local (iarray [ ival (r `bitsAt` 24)
                            , ival (r `bitsAt` 16)
                            , ival (r `bitsAt` 8)
                            , ival (r `bitsAt` 0)])
        arrayMap $ \(i :: Ix 4) ->
          emit e (constRef (vs ! i))

    where
    bitsAt :: Uint32 -> Int -> Uint8
    bitsAt v n = bitCast (v `iShiftR` (fromIntegral n))
