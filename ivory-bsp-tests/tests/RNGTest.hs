
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.RNG (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
  app testplatform_leds
      (stm32config_clock . testplatform_stm32)
      testplatform_uart
      testplatform_rng
  where
  p topts = getConfig topts testPlatformParser
