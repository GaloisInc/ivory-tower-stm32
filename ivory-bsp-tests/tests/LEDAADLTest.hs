
module Main where

import Ivory.Tower.Options

import Tower.AADL

import Ivory.Tower.Config

import Tower.AADL.Build.Common
import Tower.AADL.Build.EChronos

import Ivory.BSP.STM32.Config

import BSP.Tests.Platforms
import BSP.Tests.LED.TestApp (app)

main :: IO ()
main = compileTowerAADLForPlatform f p $ do
  app testplatform_leds
  where
  f :: TestPlatform -> (AADLConfig, OSSpecific STM32Config)
  f tp = ( defaultAADLConfig { configSystemOS  = EChronos
                             , configSystemHW  = PIXHAWK
                             }
         , defaultEChronosOS (testplatform_stm32 tp)
         )
  p :: TOpts -> IO TestPlatform
  p topts = fmap fst (getConfig' topts testPlatformParser)
