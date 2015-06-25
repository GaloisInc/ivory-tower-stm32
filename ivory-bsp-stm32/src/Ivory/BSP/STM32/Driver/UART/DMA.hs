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
import Ivory.HW.Reg
import Ivory.HW.BitData

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

  mapM_ towerArtifact dmaArtifacts

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

  monitorModuleDef $ do
    hw_moduledef
    incl ref_to_uint32_proc

  req_buf <- state "req_buf"

  handler (dma_stream_init rxstream) "init" $ callback $ const $ do
    debug_init dbg
    uartInit uart pins clockConfig (fromIntegral baud)

  handler req_chan "req_chan" $ callback $ \req -> do
    refCopy req_buf req
    -- Disable transmit stream:
    disableStream tx_regs
    -- Clear transmit stream flags:
    dma_stream_clear_isrflags txstream

    -- Set peripheral address:
    setReg (dmaStreamPAR tx_regs) $
      setField dma_sxpar_par (fromRep (bdr_reg_addr (uartRegDR uart)))

    -- Set memory address:
    buf_start_addr <- call ref_to_uint32_proc ((req_buf ~> stringDataL) ! 0)
    setReg (dmaStreamM0AR  tx_regs) $
      setField dma_sxm0ar_m0a (fromRep buf_start_addr)

    -- Set number of data items to be transfered:
    let safe_items :: Sint32 -> Uint16
        safe_items n =
          (n <? arrayLen (req_buf ~> stringDataL) .&& n >=? 0 .&& n <? 65535)
          ? (castWith 0 n, 0)
    req_items <- fmap safe_items (deref (req_buf ~> stringLengthL))
    setReg (dmaStreamNDTR tx_regs) $
      setField dma_sxndtr_ndt (fromRep req_items)

    -- Set FIFO control register:
    setReg (dmaStreamFCR tx_regs) $ do
      setBit   dma_sxfcr_dmdis
      setField dma_sxfcr_fth (fromRep 3)

    -- Set control register:
    setReg (dmaStreamCR tx_regs) $ do
      setField dma_sxcr_chsel  (fromRep (fromIntegral tx_chan))
      setField dma_sxcr_mburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_pburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_dbm    (fromRep 0) -- Single Buffering
      setField dma_sxcr_msize  (fromRep 0) -- Byte memory size
      setField dma_sxcr_psize  (fromRep 0) -- Byte peripheral size
      setField dma_sxcr_minc   (fromRep 1) -- Increment according to msize
      setField dma_sxcr_pinc   (fromRep 0) -- Fixed (no increment)
      setField dma_sxcr_circ   (fromRep 0) -- No circular mode
      setField dma_sxcr_dir    (fromRep 1) -- Memory to Peripheral
      setField dma_sxcr_pfctrl (fromRep 0) -- DMA is flow controller
      setField dma_sxcr_tcie   (fromRep 1) -- Enable transfer complete interrupt
      setField dma_sxcr_htie   (fromRep 0) -- Disable half transfer interrupt
      setField dma_sxcr_teie   (fromRep 1) -- Enable transfer error interrupt
      setField dma_sxcr_dmeie  (fromRep 1) -- Enable direct mode error interrupt

  tx_complete     <- state "tx_complete"
  tx_transfer_err <- state "tx_transfer_err"
  tx_direct_err   <- state "tx_direct_err"
  handler (dma_stream_signal txstream) "tx_stream_interrupt" $ do
    e <- emitter resp_chan 1
    callback $ const $ do
      flags <- dma_stream_get_isrflags txstream

      let incr r = r %= (+ (1 :: Uint32))
      when (bitToBool (flags #. dma_isrflag_TCIF)) $ do
        incr tx_complete
        emitV e true
      when (bitToBool (flags #. dma_isrflag_TEIF)) $ do
        incr tx_transfer_err
      when (bitToBool (flags #. dma_isrflag_DMEIF)) $ do
        incr tx_direct_err


  where
  rxstream = dmaUARTRxStream  dmauart streams
  rx_chan  = dmaUARTRxChannel dmauart
  rx_regs  = dma_stream_regs  rxstream
  txstream = dmaUARTTxStream  dmauart streams
  tx_chan  = dmaUARTTxChannel dmauart
  tx_regs  = dma_stream_regs  txstream


  uart = dmaUARTPeriph dmauart
  named n = uartName uart ++ "_dma_" ++ n

  ref_to_uint32_proc :: Def('[Ref s (Stored Uint8)] :-> Uint32)
  ref_to_uint32_proc = importProc "ref_to_uint32" dmaRefToUint32Header


  bdr_reg_addr :: BitDataReg a -> Uint32
  bdr_reg_addr = fromInteger . unReg . bdr_reg
    where unReg (Reg a) = a


