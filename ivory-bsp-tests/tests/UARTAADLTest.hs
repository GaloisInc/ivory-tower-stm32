
module Main where

import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.HW.Module

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
        mapM_ towerArtifact hw_artifacts
  where
  f :: TestPlatform -> (AADLConfig, OSSpecific STM32Config e)
  f tp = ( defaultAADLConfig { configSystemOS = EChronos
                             , configSystemHW = PIXHAWK
                             }
         , defaultEChronosOS (testplatform_stm32 tp)
         )
  p :: TOpts -> IO TestPlatform
  p topts = fmap fst (getConfig' topts testPlatformParser)
