{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BSP.Tests.UART.TestApp (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink
import BSP.Tests.UART.Types

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig

--------------------------------------------------------------------------------

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

app :: (e -> ColoredLEDs)
    -> (e -> ClockConfig)
    -> (e -> TestUART)
    -> Tower e ()
app toleds tocc touart = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes

  e <- getEnv
  -- Starts two tasks: a blink task and a controller task.  Periodically blink
  -- the blue LED.
  blink p [blueLED (toleds e)]
  -- A new queue
  redledctl <- channel
  -- Starts a UART (serial) task
  let u = touart e
  (BackpressureTransmit req res, ostream) <- uartTower tocc (testUARTPeriph u) (testUARTPins u)
                                                       115200
  -- Start the task defined below
  echoPrompt "hello world" req res ostream (fst redledctl)
  -- A task that takes control input (Boolean) from the echo prompt and controls
  -- the red LED based on it.
  monitor "settableLED" $ ledController [redLED (toleds e)] (snd redledctl)
  where
  p = Milliseconds 333

--------------------------------------------------------------------------------

echoPrompt :: String
           -> ChanInput  UARTBuffer
           -> ChanOutput (Stored IBool)
           -> ChanOutput (Stored Uint8)
           -> ChanInput  (Stored IBool)
           -> Tower p ()
echoPrompt greeting req res ostream ledctl = do
  p <- period (Milliseconds 1)

  monitor "echoprompt" $ do
    (out_req :: Ref Global UARTBuffer) <- state "out_req"

    let puts :: String -> Ivory eff ()
        puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str

        putc :: Uint8 -> Ivory eff ()
        putc byte = do
          pos <- deref (out_req ~> stringLengthL)
          when (pos <? arrayLen (out_req ~> stringDataL)) $ do
            store (out_req ~> stringDataL ! toIx pos) byte
            store (out_req ~> stringLengthL) (pos + 1)

        flush :: (GetAlloc eff ~ Scope cs)
              => Emitter UARTBuffer -> Ivory eff ()
        flush e = do
          emit e (constRef out_req)
          store (out_req ~> stringLengthL) 0

    initialized <- stateInit "initialized" (ival false)

    handler p "init" $ do
      e <- emitter req 1
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts (greeting ++ "\r\n")
          puts prompt
          flush e

    handler ostream "ostream" $ do
      e <- emitter req 1
      l <- emitter ledctl 1
      callbackV $ \input -> do
        -- Echo to terminal, replace newline with CR/LF for portability.
        ifte_ (isChar input '\n' .|| isChar input '\r')
          (puts $ "\r\n" ++ prompt)
          (putc input)
        let testChar = (input `isChar`)
        cond_
          [ testChar '1'  ==> emitV l true
          , testChar '2'  ==> emitV l false
          ]
        flush e

    handler res "result" $ return () -- XXX
  where prompt = "tower> "

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

