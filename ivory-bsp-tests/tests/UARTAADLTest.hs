
module Main where

import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.Tower
import Ivory.HW.Module

import Tower.AADL

import BSP.Tests.Platforms
import BSP.Tests.UART.TestApp (app)

main :: IO ()
main = compileTowerAADL f p $ do
        app testplatform_leds
            (stm32config_clock . testplatform_stm32)
            testplatform_uart
        mapM_ towerArtifact hw_artifacts
  where
  f :: TestPlatform -> AADLConfig
  f _ = defaultAADLConfig { configSystemOS = EChronos, configSystemHW = PIXHAWK
                          , configLibDir   = "" }
  -- TODO JED: This test should really use the testPlatformParser, but first we
  -- need to fix a bug in the interaction between the testPlatformParser and
  -- the option parser used inside compileTowerAADL. For now the compromise
  -- is to simply hardcode the platform.
  p _ = return px4fmuv24
  -- p topts = getConfig topts testPlatformParser
