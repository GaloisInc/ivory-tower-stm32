{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.UART.TestApp (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import BSP.Tests.Platforms
import BSP.Tests.LED.Blink

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

--------------------------------------------------------------------------------

app :: (e -> ColoredLEDs)
    -> (e -> ClockConfig)
    -> (e -> TestUART F405.Interrupt)
    -> Tower e ()
app toleds tocc touart = do
  e <- getEnv
  -- Starts two tasks: a blink task and a controller task.  Periodically blink
  -- the blue LED.
  blink p [blueLED (toleds e)]
  -- A new queue
  redledctl <- channel
  -- Starts a UART (serial) task
  (istream, ostream) <- uartTower tocc (testUART (touart e)) 115200 (Proxy :: Proxy 256)
  -- Start the task defined below
  echoPrompt "hello world" ostream istream (fst redledctl)
  -- A task that takes control input (Boolean) from the echo prompt and controls
  -- the red LED based on it.
  monitor "settableLED" $ ledController [redLED (toleds e)] (snd redledctl)
  where
  p = Milliseconds 333

--------------------------------------------------------------------------------

echoPrompt :: String
           -> ChanInput  (Stored Uint8)
           -> ChanOutput (Stored Uint8)
           -> ChanInput  (Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctl = monitor "echoprompt" $ do

  let puts :: (GetAlloc eff ~ Scope cs)
           => Emitter (Stored Uint8) -> String -> Ivory eff ()
      puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ Scope cs)
           => Emitter (Stored Uint8) -> Uint8 -> Ivory eff ()
      putc = emitV

  handler systemInit "init" $ do
    o <- emitter ostream 32
    callback $ const $ do
      puts o (greeting ++ "\n")
      puts o prompt

  handler istream "istream" $ do
    l <- emitter ledctl 1
    o <- emitter ostream 32
    callbackV $ \input -> do
      putc o input -- echo to terminal
      let testChar = (input `isChar`)
      cond_
        [ testChar '1'  ==> emitV l true
        , testChar '2'  ==> emitV l false
        , testChar '\n' ==> puts o prompt
        ]
  where prompt = "tower> "

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

