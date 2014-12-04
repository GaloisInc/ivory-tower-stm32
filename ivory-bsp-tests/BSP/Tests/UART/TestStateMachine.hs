{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.UART.TestStateMachine (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.Tower.StateMachine.Example

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

--------------------------------------------------------------------------------

app :: (e -> ClockConfig)
    -> (e -> TestUART F405.Interrupt)
    -> Tower e ()
app tocc touart = do
  e <- getEnv
  -- Starts a UART (serial) task
  (istream, ostream) <- uartTower tocc (testUART (touart e)) 115200 (Proxy :: Proxy 256)
  -- Start the task defined below
  smTestMonitor ostream istream
  where
  p = Milliseconds 333

--------------------------------------------------------------------------------

smTestMonitor :: ChanInput  (Stored Uint8)
              -> ChanOutput (Stored Uint8)
              -> Tower p ()
smTestMonitor ostream istream = monitor "smTestMonitor" $ do
  echo_ex istream ostream

