
module Main where

import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import BSP.Tests.Platforms
import BSP.Tests.LED.TestApp (app)

main :: IO ()
main = towerCompile p (app testplatform_leds)
  where
  -- TODO: this is where tower-config should be integrated.
  p topts = do
    putStrLn (show (topts_args topts))
    return $ stm32FreeRTOS testplatform_stm32 px4fmuv17

