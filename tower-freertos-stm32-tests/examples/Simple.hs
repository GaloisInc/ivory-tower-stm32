{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

-- Just using the PlatformClock constraint to make sure it works.
import Ivory.BSP.STM32.ClockConfig

test1_per :: (e -> ClockConfig) -> Tower e ()
test1_per _tocc = do
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
main = compileTowerSTM32FreeRTOS id p (test1_per stm32config_clock)
  where p topts = getConfig topts $ stm32ConfigParser $ stm32f405Defaults 24
