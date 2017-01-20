
module Main where

import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.Tower.Config
import Ivory.Tower.Options

import Tower.AADL

import Tower.AADL.Build.Common
import Tower.AADL.Build.EChronos

import BSP.Tests.Platforms
import BSP.Tests.UART.TestApp (app)

main :: IO ()
main = compileTowerAADLForPlatform f p $ do
        app testplatform_leds
            (stm32config_clock . testplatform_stm32)
            testplatform_uart
  where
  f :: TestPlatform -> (AADLConfig, OSSpecific STM32Config)
  f tp = ( defaultAADLConfig { configSystemOS  = EChronos
                             , configSystemHW  = PIXHAWK
                             }
         , defaultEChronosOS (testplatform_stm32 tp)
         )
  p :: TOpts -> IO TestPlatform
  p topts = fmap fst (getConfig' topts testPlatformParser)
