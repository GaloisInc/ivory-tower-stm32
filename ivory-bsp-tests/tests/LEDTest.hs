
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.LED.TestApp (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
  app testplatform_leds
  where
  p topts = getConfig topts testPlatformParser

