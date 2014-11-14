{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.LED.TestApp where

import Ivory.Tower
import BSP.Tests.Platforms
import BSP.Tests.LED.Blink

app :: (e -> ColoredLEDs) -> Tower e ()
app toleds = do
  leds <- fmap toleds getEnv
  blink (Milliseconds 250) [redLED leds]
  blink (Milliseconds 333) [blueLED leds]
