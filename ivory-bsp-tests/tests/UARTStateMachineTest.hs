
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.UART.TestStateMachine (app)

main :: IO ()
main = towerCompile p (app (stm32config_clock . testplatform_stm32)
                           testplatform_uart)
  where
  p topts = do
    cfg <- getConfig topts testPlatformParser
    return $ stm32FreeRTOS testplatform_stm32 cfg
