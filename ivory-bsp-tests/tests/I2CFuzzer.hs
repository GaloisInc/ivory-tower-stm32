{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Meant for debugging the I2C driver by sending noise out on the
-- I2C bus. Helps to exercise the error handling and reset
-- functionality of the driver.
module Main where

import Control.Monad (replicateM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.HW

import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.RNG
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32F427.GPIO
import Ivory.BSP.STM32F427.RNG

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
  rngs <- rngTower rng (Milliseconds 1)
  p <- period (Milliseconds 90)

  monitor "i2c_fuzzer" $ do
    monitorModuleDef hw_moduledef
    next_delay <- state "next_delay"
    next_run <- state "next_run"
    sda_pattern <- state "sda_pattern"
    scl_pattern <- state "scl_pattern"

    handler systemInit "init" $
      callback $ \_ -> do
        setupPin sda
        setupPin scl

    handler p "chaos" $ do
      callback $ \_ -> do
        t_next <- deref next_run
        t <- getTime
        unless (t >? t_next .|| t_next ==? 0) $ do
          (sda_pat :: Uint32) <- deref sda_pattern
          (scl_pat :: Uint32) <- deref scl_pattern
          for 32 $ \(ix :: Ix 33) -> do
            let sda_on = ((sda_pat `iShiftR` (signCast (fromIx ix))) .& 1) ==? 1
                scl_on = ((scl_pat `iShiftR` (signCast (fromIx ix))) .& 1) ==? 1
            ifte_ sda_on
              (replicateM_ 16 (pinSet sda))
              (replicateM_ 16 (pinClear sda))
            ifte_ scl_on
              (replicateM_ 16 (pinSet scl))
              (replicateM_ 16 (pinClear scl))
          pinClear sda
          pinClear scl
          d_next  <- deref next_delay
          t' <- getTime
          store next_run (t' + d_next)

    coroutineHandler systemInit rngs "use_rng" $ do
      return $ CoroutineBody $ \yield -> do
        -- 5ms minimum delay
        let rngToTime x = fromIMicroseconds ((x `iDiv` 1000) + 5000)
            loop = do
              delay <- deref =<< yield
              store next_delay (rngToTime delay)
              store sda_pattern =<< deref =<< yield
              store scl_pattern =<< deref =<< yield
        -- run through once outside the loop to set up the next_run
        loop
        delay0 <- deref next_delay
        t <- getTime
        store next_run (t + delay0)
        -- then start the loop
        forever loop

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p app
  where
  p :: TOpts -> IO TestPlatform
  p topts = getConfig topts testPlatformParser
