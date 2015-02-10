{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Driver.UART
  ( uartTower
  , uartTowerDebuggable
  , UARTTowerDebugger(..)
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART.Regs
import Ivory.BSP.STM32.Peripheral.UART.Peripheral
import Ivory.BSP.STM32.Driver.RingBuffer

data UARTTowerDebugger =
  UARTTowerDebugger
    { debug_init             :: forall eff . Ivory eff ()
    , debug_isr              :: forall eff . Ivory eff ()
    , debug_evthandler_start :: forall eff . Ivory eff ()
    , debug_evthandler_end   :: forall eff . Ivory eff ()
    , debug_txcheck          :: forall eff . Ivory eff ()
    , debug_txcheck_pend     :: forall eff . Ivory eff ()
    , debug_txeie            :: forall eff . IBool -> Ivory eff ()
    }

emptyDbg :: UARTTowerDebugger
emptyDbg =
  UARTTowerDebugger
    { debug_init = return ()
    , debug_isr  = return ()
    , debug_evthandler_start = return ()
    , debug_evthandler_end = return ()
    , debug_txcheck = return ()
    , debug_txcheck_pend = return ()
    , debug_txeie = const (return ())
    }

uartTower :: (STM32Interrupt s, ANat n)
          => (e -> ClockConfig)
          -> UART s
          -> UARTPins
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower e ( ChanOutput (Stored Uint8)
                     , ChanInput  (Stored Uint8))
uartTower tocc u p b s = uartTowerDebuggable tocc u p b s emptyDbg

uartTowerDebuggable :: (STM32Interrupt s, ANat n)
                    => (e -> ClockConfig)
                    -> UART s
                    -> UARTPins
                    -> Integer
                    -> Proxy (n :: Nat)
                    -> UARTTowerDebugger
                    -> Tower e ( ChanOutput (Stored Uint8)
                               , ChanInput  (Stored Uint8))
uartTowerDebuggable tocc uart pins baud sizeproxy dbg = do

  (src_ostream, snk_ostream) <- channel
  (src_istream, snk_istream) <- channel

  interrupt <- signalUnsafe
    (Interrupt (uartInterrupt uart))
    (Microseconds 50) -- XXX calculate from baud rate.
    (do debug_isr dbg
        --setTXEIE uart false
        --setRXNEIE uart false
        interrupt_disable (uartInterrupt uart))

  monitor (uartName uart ++ "_driver") $ do
    uartTowerMonitor tocc uart pins baud sizeproxy interrupt snk_ostream src_istream dbg

  return (snk_istream, src_ostream)

uartTowerMonitor :: forall e n s
                  . (ANat n, STM32Interrupt s)
                 => (e -> ClockConfig)
                 -> UART s
                 -> UARTPins
                 -> Integer
                 -> Proxy (n :: Nat)
                 -> ChanOutput (Stored ITime)
                 -> ChanOutput (Stored Uint8)
                 -> ChanInput (Stored Uint8)
                 -> UARTTowerDebugger
                 -> Monitor e ()
uartTowerMonitor tocc uart pins baud _ interrupt ostream istream dbg = do
  clockConfig <- fmap tocc getEnv

  monitorModuleDef $ hw_moduledef

  rxoverruns    <- stateInit (named "rx_overruns") (ival (0 :: Uint32))
  rxsuccess     <- stateInit (named "rx_success") (ival (0 :: Uint32))


  handler systemInit "init" $ callback $ const $ do
    debug_init dbg
    uartInit uart pins clockConfig (fromIntegral baud)

  (outbuf :: RingBuffer n (Stored Uint8)) <- monitorRingBuffer "outbuf"

  let pop :: Ref s' (Stored Uint8) -> Ivory eff IBool
      pop b = ringbuffer_pop outbuf b

  handler ostream "ostream" $ callback $ \b -> do
    _ <- ringbuffer_push outbuf b
    setTXEIE uart true

  handler interrupt "interrupt" $ do
    i <- emitter istream 1
    callback $ const $ do
      debug_evthandler_start dbg
      sr <- getReg (uartRegSR uart)
      when (bitToBool (sr #. uart_sr_orne)) $ do
        byte <- readDR uart
        bref <- local (ival byte)
        emit i (constRef bref)
        rxoverruns %= (+1) -- This is basically an error we can't handle, but its
                           -- useful to be able to check them with gdb
      when (bitToBool (sr #. uart_sr_rxne)) $ do
        byte <- readDR uart
        bref <- local (ival byte)
        emit i (constRef bref)
        rxsuccess %= (+1) -- For debugging
      when (bitToBool (sr #. uart_sr_txe)) $ do
        byte <- local (ival 0)
        rv   <- pop byte
        when rv $ do
          tosend <- deref byte
          setDR uart tosend
      debug_evthandler_end dbg
      txdone <- ringbuffer_empty outbuf
      setTXEIE uart (iNot txdone)
      interrupt_enable (uartInterrupt uart)

  where named n = uartName uart ++ "_" ++ n

