
module Main where

import Tower.Config
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.I2C.TestApp (app)

main :: IO ()
main = towerCompile p (app (stm32config_clock . testplatform_stm32)
                           testplatform_i2c)
  where
  p topts = do
    cfg <- getConfig topts testPlatformParser
    return $ stm32FreeRTOS testplatform_stm32 cfg

