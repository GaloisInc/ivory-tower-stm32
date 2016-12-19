-- | Meant for debugging the I2C driver by sending noise out on the
-- I2C bus. Helps to exercise the error handling and reset
-- functionality of the driver.
module Main where

import Control.Monad (replicateM_)

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.HW

import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32F427.GPIO
import Ivory.BSP.STM32.Peripheral.GPIOF4

import BSP.Tests.Platforms

sda, scl :: GPIOPin
sda = pinB8
scl = pinB9

setupPin :: GPIOPin -> Ivory eff ()
setupPin p = do
  pinEnable        p
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_100mhz
  pinSetPUPD       p gpio_pupd_pullup
  pinClear         p
  pinSetMode       p gpio_mode_output

app :: Tower e ()
app = do
  monitor "i2c_fuzzer" $ do
    monitorModuleDef hw_moduledef
    handler systemInit "init" $
      callback $ \_ -> do
        setupPin sda
        setupPin scl
        forever $ do
          replicateM_ 16 (pinSet sda >> pinSet scl)
          replicateM_ 16 (pinClear sda >> pinClear scl)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p app
  where
  p :: TOpts -> IO TestPlatform
  p topts = getConfig topts testPlatformParser
