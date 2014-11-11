{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import System.Exit (exitFailure)
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Compile
import Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

-- Just using the PlatformClock constraint as a test
import Ivory.BSP.STM32.PlatformClock

import qualified Ivory.Compile.C.CmdlineFrontend as C

instance PlatformClock Config where
  getClockConfig = config_clock `fmap` getEnv

test1_per :: (PlatformClock e) => Tower e ()
test1_per = do
  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "last_m1_tick_message"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \m -> do
        refCopy s m
        emit e m
  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

main :: IO ()
main =  do
  c <- configFromFile "default.conf" ["."]
  case c of
    Right conf -> do
      let platform = stm32FreeRTOS conf
      runTowerCompile test1_per platform copts
    Left e -> putStrLn e >> exitFailure
  where
  copts = C.initialOpts { C.outDir = Just "tower-example-simple" }
