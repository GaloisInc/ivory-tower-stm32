
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.I2C.TestApp (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_i2c
  where
  p topts = getConfig topts testPlatformParser

