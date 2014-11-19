{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Driver.UART
  ( uartTower
  --, uartTowerFlushable
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
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower e ( ChanOutput (Stored Uint8)
                     , ChanInput  (Stored Uint8))
uartTower tocc u b s = uartTowerDebuggable tocc u b s emptyDbg

{-
uartTowerFlushable :: forall n p
           . (ANat n, Env ClockConfig p, STM32Signal p)
          => UART (InterruptType p)
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower p ( ChanOutput   (Stored Uint8)
                     , ChanInput (Stored Uint8)
                     , ChanInput (Stored ITime))
uartTowerFlushable uart baud sizeproxy = do
  (src_ostream, snk_ostream) <- channel
  (src_istream, snk_istream) <- channel
  (src_flush, snk_flush) <- channel

  monitor (uartName uart ++ "_flushable_driver") $ do
    txcheck_evt <- withChannelEvent snk_flush "flush"
    uartTowerMonitor uart baud snk_ostream src_istream txcheck_evt emptyDbg

  return (snk_istream, src_ostream, src_flush)
-}

uartTowerDebuggable :: (STM32Interrupt s, ANat n)
                    => (e -> ClockConfig)
                    -> UART s
                    -> Integer
                    -> Proxy (n :: Nat)
                    -> UARTTowerDebugger
                    -> Tower e ( ChanOutput (Stored Uint8)
                               , ChanInput  (Stored Uint8))
uartTowerDebuggable tocc uart baud sizeproxy dbg = do

  (src_ostream, snk_ostream) <- channel
  (src_istream, snk_istream) <- channel

  txcheck_evt <- period txcheck_period
  interrupt <- signalUnsafe
    (Interrupt (uartInterrupt uart))
    (Microseconds 50)
    (do debug_isr dbg
        setTXEIE uart false
        setRXNEIE uart false
        interrupt_disable (uartInterrupt uart))

  monitor (uartName uart ++ "_driver") $ do
    uartTowerMonitor tocc uart baud sizeproxy interrupt snk_ostream src_istream txcheck_evt dbg

  return (snk_istream, src_ostream)

  where
  txcheck_period = Milliseconds 1


uartTowerMonitor :: forall e n s
                  . (ANat n, STM32Interrupt s)
                 => (e -> ClockConfig)
                 -> UART s
                 -> Integer
                 -> Proxy (n :: Nat)
                 -> ChanOutput (Stored ITime)
                 -> ChanOutput (Stored Uint8)
                 -> ChanInput (Stored Uint8)
                 -> ChanOutput (Stored ITime)
                 -> UARTTowerDebugger
                 -> Monitor e ()
uartTowerMonitor tocc uart baud _ interrupt ostream istream txcheck_evt dbg = do
  clockConfig <- fmap tocc getEnv
  -- XXX need internal queue for ostream:

  monitorModuleDef $ hw_moduledef

  rxoverruns    <- stateInit (named "rx_overruns") (ival (0 :: Uint32))
  rxsuccess     <- stateInit (named "rx_success") (ival (0 :: Uint32))
  txpending     <- state (named "tx_pending")
  txpendingbyte <- state (named "tx_pendingbyte")


  handler systemInit "init" $ callback $ const $ do
    debug_init dbg
    store txpending false
    uartInit    uart clockConfig (fromIntegral baud)
    uartInitISR uart

  (outbuf :: RingBuffer n (Stored Uint8)) <- monitorRingBuffer "outbuf"

  handler ostream "ostream" $ callback $ \b -> do
    _ <- ringbuffer_push outbuf b
    return ()

  let pop :: Ref s' (Stored Uint8) -> Ivory eff IBool
      pop b = ringbuffer_pop outbuf b

  handler interrupt "interrupt" $ do
    i <- emitter istream 1
    callback $ const $ do
      debug_evthandler_start dbg
      continueTXEIE <- local (ival false)
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
        pending <- deref txpending
        ifte_ pending
          (do store txpending false
              tosend <- deref txpendingbyte
              store continueTXEIE true
              setDR uart tosend)
          (do byte <- local (ival 0)
              rv   <- pop byte
              when rv $ do
                tosend <- deref byte
                store continueTXEIE true
                setDR uart tosend)
      debug_evthandler_end dbg
      setTXEIE uart =<< deref continueTXEIE
      setRXNEIE uart true
      interrupt_enable (uartInterrupt uart)

  handler txcheck_evt "txcheck" $ callback $ \_ -> do
    txeie <- getTXEIE uart
    pending <- deref txpending
    unless (txeie .&& iNot pending) $ do
      debug_txcheck dbg
      byte <- local (ival 0)
      txready <- pop byte
      when txready $ do
        debug_txcheck_pend dbg
        store txpending true
        store txpendingbyte =<< deref byte
        setTXEIE uart true


  where named n = (uartName uart) ++ "_"++ n

