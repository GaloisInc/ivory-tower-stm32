{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Driver.UART.DMA
  ( dmaUARTTower
  , dmaUARTTowerDebuggable
  , DMAUARTTowerDebugger(..)
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART.Regs
import Ivory.BSP.STM32.Peripheral.UART.Peripheral
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.Peripheral.DMA

import Ivory.BSP.STM32.Driver.DMA

data DMAUARTTowerDebugger =
  DMAUARTTowerDebugger
    { debug_init             :: forall eff . Ivory eff ()
    , debug_isr              :: forall eff . Ivory eff ()
    , debug_evthandler_start :: forall eff . Ivory eff ()
    , debug_evthandler_end   :: forall eff . Ivory eff ()
    , debug_txcheck          :: forall eff . Ivory eff ()
    , debug_txcheck_pend     :: forall eff . Ivory eff ()
    , debug_txeie            :: forall eff . IBool -> Ivory eff ()
    }

emptyDbg :: DMAUARTTowerDebugger
emptyDbg =
  DMAUARTTowerDebugger
    { debug_init = return ()
    , debug_isr  = return ()
    , debug_evthandler_start = return ()
    , debug_evthandler_end = return ()
    , debug_txcheck = return ()
    , debug_txcheck_pend = return ()
    , debug_txeie = const (return ())
    }

dmaUARTTower :: (IvoryString tx, IvoryString rx)
             => (e -> ClockConfig)
             -> DMAUART
             -> UARTPins
             -> DMATowerStreams
             -> Integer
             -> Tower e ( BackpressureTransmit tx (Stored IBool)
                        , ChanOutput rx)
dmaUARTTower tocc u p s b = dmaUARTTowerDebuggable tocc u p s b emptyDbg

dmaUARTTowerDebuggable :: (IvoryString tx, IvoryString rx)
                       => (e -> ClockConfig)
                       -> DMAUART
                       -> UARTPins
                       -> DMATowerStreams
                       -> Integer
                       -> DMAUARTTowerDebugger
                       -> Tower e ( BackpressureTransmit tx (Stored IBool)
                                  , ChanOutput rx)
dmaUARTTowerDebuggable tocc dmauart pins streams baud dbg = do
  req_chan  <- channel
  resp_chan <- channel
  rx_chan   <- channel

  monitor (uartName uart ++ "_dma_driver") $ do
    dmaUARTTowerMonitor tocc dmauart pins streams baud
                        (fst rx_chan) (snd req_chan) (fst resp_chan) dbg
  return (BackpressureTransmit (fst req_chan) (snd resp_chan), (snd rx_chan))
  where
  uart = dmaUARTPeriph dmauart

dmaUARTTowerMonitor :: (IvoryString tx, IvoryString rx)
                    => (e -> ClockConfig)
                    -> DMAUART
                    -> UARTPins
                    -> DMATowerStreams
                    -> Integer
                    -> ChanInput rx
                    -> ChanOutput tx
                    -> ChanInput (Stored IBool)
                    -> DMAUARTTowerDebugger
                    -> Monitor e ()
dmaUARTTowerMonitor tocc dmauart pins streams baud rx_chan req_chan resp_chan dbg = do
  clockConfig <- fmap tocc getEnv

  monitorModuleDef $ hw_moduledef

  req_buf <- state "req_buf"

  handler (dma_stream_init rxstream) "init" $ callback $ const $ do
    debug_init dbg
    uartInit uart pins clockConfig (fromIntegral baud)

  handler req_chan "req_chan" $ callback $ \req -> do
    refCopy req_buf req


  where
  rxstream = dmaUARTRxStream  dmauart streams
  rxchan   = dmaUARTRxChannel dmauart
  txstream = dmaUARTTxStream  dmauart streams
  txchan   = dmaUARTTxChannel dmauart


  uart = dmaUARTPeriph dmauart
  named n = uartName uart ++ "_dma_" ++ n
