{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.UART.TestStateMachine (app) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine.Example

import BSP.Tests.Platforms

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
  let u = touart e
  (istream, ostream) <- uartTower tocc (testUARTPeriph u) (testUARTPins u)
                                  115200 (Proxy :: Proxy 256)
  -- Start the task defined below
  smTestMonitor ostream istream

--------------------------------------------------------------------------------

smTestMonitor :: ChanInput  (Stored Uint8)
              -> ChanOutput (Stored Uint8)
              -> Tower p ()
smTestMonitor ostream istream = monitor "smTestMonitor" $ do
  -- Echoes all characters to the screen immediately, unless it recieves a '\n'
  -- character, in which case it waits 1 second, dropping all characters
  -- received while waiting, and prints a '\n'.  It then returns to echoing all
  -- characters immediately.
  echo_ex istream ostream

