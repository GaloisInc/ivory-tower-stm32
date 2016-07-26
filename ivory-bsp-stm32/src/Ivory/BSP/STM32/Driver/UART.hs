{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Driver.UART
  ( uartTower
  , uartTowerDebuggable
  , UARTTowerDebugger(..)
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART.Regs
import Ivory.BSP.STM32.Peripheral.UART.Peripheral

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

uartTower :: IvoryString s
          => (e -> ClockConfig)
          -> UART
          -> UARTPins
          -> Integer
          -> Tower e ( BackpressureTransmit s ('Stored IBool)
                     , ChanOutput ('Stored Uint8)
                     , Monitor e ())
uartTower tocc u p b = uartTowerDebuggable tocc u p b emptyDbg

uartTowerDebuggable :: IvoryString s
                    => (e -> ClockConfig)
                    -> UART
                    -> UARTPins
                    -> Integer
                    -> UARTTowerDebugger
                    -> Tower e ( BackpressureTransmit s ('Stored IBool)
                               , ChanOutput ('Stored Uint8)
                               , Monitor e ())
uartTowerDebuggable tocc uart pins baud dbg = do
  req_chan  <- channel
  resp_chan <- channel
  rx_chan   <- channel

  interrupt <- signalUnsafe
    (Interrupt (uartInterrupt uart))
    (Microseconds 50) -- XXX calculate from baud rate.
    (do debug_isr dbg
        interrupt_disable (uartInterrupt uart))

  let mon = uartTowerMonitor tocc uart pins baud interrupt (fst rx_chan) (snd req_chan) (fst resp_chan) dbg

  return (BackpressureTransmit (fst req_chan) (snd resp_chan), (snd rx_chan), mon)

uartTowerMonitor :: IvoryString s
                 => (e -> ClockConfig)
                 -> UART
                 -> UARTPins
                 -> Integer
                 -> ChanOutput ('Stored ITime)
                 -> ChanInput ('Stored Uint8)   -- byte at a time rx
                 -> ChanOutput s
                 -> ChanInput ('Stored IBool)
                 -> UARTTowerDebugger
                 -> Monitor e ()
uartTowerMonitor tocc uart pins baud interrupt rx_chan req_chan resp_chan dbg = do
  clockConfig <- fmap tocc getEnv

  monitorModuleDef $ hw_moduledef

  rxoverruns    <- stateInit (named "rx_overruns") (ival (0 :: Uint32))
  rxsuccess     <- stateInit (named "rx_success") (ival (0 :: Uint32))

  req_buf <- state "req_buf"
  req_pos <- state "req_pos"

  handler systemInit "init" $ callback $ const $ do
    debug_init dbg
    uartInit uart pins clockConfig (fromIntegral baud) True

  let req_pop_byte :: (GetAlloc eff ~ 'Scope cs)
                   => Ref s' ('Stored Uint8) -> Ivory eff IBool
      req_pop_byte b = do
        result <- local (ival false)
        pos <- deref req_pos
        len <- istr_len (constRef req_buf)
        buf <- assign (req_buf ~> stringDataL)

        when (pos <? len) $ do
          byte <- deref (buf ! toIx pos)
          store b byte
          store req_pos (pos + 1)
          store result true

        deref result

  handler req_chan "req_chan" $ callback $ \req -> do
    refCopy req_buf req
    store req_pos (0 :: Sint32)
    setTXEIE uart true

  handler interrupt "interrupt" $ do
    i <- emitter rx_chan 1
    resp_emitter <- emitter resp_chan 1
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
        rv   <- req_pop_byte byte
        ifte_ rv
          (do tosend <- deref byte
              setDR uart tosend)
          (do -- Ensure we only send the response message
              -- when we have completed sending a valid buffer
              tx_len <- deref (req_buf ~> stringLengthL)
              valid_tx_buf <- assign (tx_len >? 0)
              when valid_tx_buf $ emitV resp_emitter true
              store (req_buf ~> stringLengthL) 0
              setTXEIE uart false)
      debug_evthandler_end dbg
      interrupt_enable (uartInterrupt uart)   -- XXX needed?

  where named n = uartName uart ++ "_" ++ n

