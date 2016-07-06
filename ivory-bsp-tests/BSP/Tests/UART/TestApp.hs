{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BSP.Tests.UART.TestApp (app, echoPrompt) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig

--------------------------------------------------------------------------------


app :: (e -> ColoredLEDs)
    -> (e -> ClockConfig)
    -> (e -> TestUART)
    -> Tower e ()
app toleds tocc touart = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes

  e <- getEnv
  -- A new message channel
  (ledctl_input, ledctl_output) <- channel

  -- Starts a UART (serial) driver
  let u = touart e
  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph u) (testUARTPins u)
                                                       115200
  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  -- Start the user interaction monitor defined below
  echoPrompt "hello world" ostream istream ledctl_input

  -- A monitor that takes control output (Boolean) from the echo prompt and
  -- controls the red LED
  monitor "settableLED" $ ledController [redLED (toleds e)] ledctl_output

--------------------------------------------------------------------------------
echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctl = do
  p <- period (Milliseconds 1)

  let puts :: (GetAlloc eff ~ 'Scope cs)
           => Emitter ('Stored Uint8) -> String -> Ivory eff ()
      puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ 'Scope cs)
           => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
      putc = emitV

  monitor "echoprompt" $ do
    initialized <- stateInit "initialized" (ival false)
    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o (greeting ++ "\n")
          puts o prompt

    handler istream "istream" $ do
      l <- emitter ledctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        let testChar = (input `isChar`)
        cond_
          [ testChar '1'  ==> puts o "\r\noutput on\r\n"  >> emitV l true
          , testChar '2'  ==> puts o "\r\noutput off\r\n" >> emitV l false
          , testChar '\n' ==> puts o prompt
          ]
  where prompt = "tower> "

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

