{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Driver.UART.DMA
  ( dmaUARTTower
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


dmaUARTTower :: (IvoryString tx, IvoryString rx, Time t)
             => (e -> ClockConfig)
             -> DMAUART
             -> UARTPins
             -> DMATowerStreams
             -> Integer
             -> t
             -> Tower e ( BackpressureTransmit tx (Stored IBool)
                        , ChanOutput rx)
dmaUARTTower tocc dmauart pins streams baud rx_flush_per = do
  req_chan  <- channel
  resp_chan <- channel
  rx_chan   <- channel
  p <- period rx_flush_per

  mapM_ towerArtifact dmaArtifacts

  monitor (uartName uart ++ "_dma_driver") $ do
    dmaUARTTowerMonitor tocc dmauart pins streams baud
                        (fst rx_chan) p (snd req_chan) (fst resp_chan)
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
                    -> ChanOutput (Stored ITime)
                    -> ChanOutput tx
                    -> ChanInput (Stored IBool)
                    -> Monitor e ()
dmaUARTTowerMonitor tocc dmauart pins streams baud rx_out_chan rx_flush_chan req_chan resp_chan = do
  clockConfig <- fmap tocc getEnv

  monitorModuleDef $ do
    hw_moduledef
    incl ref_to_uint32_proc

  req_buf <- state (named "req_buf")

  rx0_buf <- state (named "rx0_buf")
  rx1_buf <- state (named "rx1_buf")

  handler (dma_stream_init rxstream) "init" $ callback $ const $ do
    uartInit uart pins clockConfig (fromIntegral baud) False
    -- Enable TX interrupt:
    dma_stream_enable_int txstream

    --------------------
    -- Setup Recieve:

    -- Set peripheral address:
    setReg (dmaStreamPAR rx_regs) $
      setField dma_sxpar_par (fromRep (bdr_reg_addr (uartRegDR uart)))

    -- Set memory address:
    buf0_start_addr <- call ref_to_uint32_proc ((rx0_buf ~> stringDataL) ! 0)
    setReg (dmaStreamM0AR  rx_regs) $
      setField dma_sxm0ar_m0a (fromRep buf0_start_addr)
    buf1_start_addr <- call ref_to_uint32_proc ((rx1_buf ~> stringDataL) ! 0)
    setReg (dmaStreamM1AR  rx_regs) $
      setField dma_sxm1ar_m1a (fromRep buf1_start_addr)

    -- Set number of data items to be transfered:
    req_items <- assign (arrayLen (rx0_buf ~> stringDataL) - 1)
    setReg (dmaStreamNDTR rx_regs) $
      setField dma_sxndtr_ndt (fromRep req_items)

    -- Set FIFO control register:
    modifyReg (dmaStreamFCR rx_regs) $ do
      setBit   dma_sxfcr_dmdis
      setField dma_sxfcr_fth (fromRep 3)

    -- Set control register:
    modifyReg (dmaStreamCR rx_regs) $ do
      setField dma_sxcr_chsel  (fromRep (fromIntegral rx_chan))
      setField dma_sxcr_mburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_pburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_ct     (fromRep 0) -- Current Target buf 0
      setField dma_sxcr_dbm    (fromRep 1) -- Double Buffering
      setField dma_sxcr_pl     (fromRep 0) -- Priority 0 (highest)
      setField dma_sxcr_msize  (fromRep 0) -- Byte memory size
      setField dma_sxcr_psize  (fromRep 0) -- Byte peripheral size
      setField dma_sxcr_minc   (fromRep 1) -- Increment according to msize
      setField dma_sxcr_pinc   (fromRep 0) -- Fixed (no increment)
      setField dma_sxcr_circ   (fromRep 0) -- No circular mode
      setField dma_sxcr_dir    (fromRep 0) -- Peripheral to Memory
      setField dma_sxcr_pfctrl (fromRep 0) -- DMA is flow controller
      setField dma_sxcr_tcie   (fromRep 1) -- Enable transfer complete interrupt
      setField dma_sxcr_htie   (fromRep 0) -- Disable half transfer interrupt
      setField dma_sxcr_teie   (fromRep 1) -- Enable transfer error interrupt
      setField dma_sxcr_dmeie  (fromRep 1) -- Enable direct mode error interrupt
    -- Enable DMA Recieve in UART:
    modifyReg (uartRegCR3 uart) $ do
      setField uart_cr3_dmar (fromRep 1)

    -- Set control register:
    modifyReg (dmaStreamCR rx_regs) $ do
      setField dma_sxcr_en  (fromRep 1) -- Enable Stream

    -- Enable RX interrupt:
    dma_stream_enable_int rxstream

  -- Debugging states:
  rx_complete     <- state (named "rx_complete")
  rx_transfer_err <- state (named "rx_transfer_err")
  rx_direct_err   <- state (named "rx_direct_err")

  handler (dma_stream_signal rxstream) "rx_stream_interrupt" $ do
    e <- emitter rx_out_chan 1
    callback $ const $ do
      -- Check stream flags:
      flags <- dma_stream_get_isrflags rxstream

      let incr r = r %= (+ (1 :: Uint32))
      when (bitToBool (flags #. dma_isrflag_TCIF)) $ do
        incr rx_complete
      when (bitToBool (flags #. dma_isrflag_TEIF)) $ do
        incr rx_transfer_err
      when (bitToBool (flags #. dma_isrflag_DMEIF)) $ do
        incr rx_direct_err

      -- Clear stream flags:
      dma_stream_clear_isrflags rxstream

      -- XXX placeholder just to infer type of rx0 and rx1 bufs:
      ifte_ false
        (emit e (constRef rx0_buf))
        (emit e (constRef rx1_buf))

      -- Enable RX interrupt:
      dma_stream_enable_int rxstream

  handler rx_flush_chan "rx_flush" $ do
    e <- emitter rx_out_chan 1
    callback $ const $ do
      -- Disable receive stream:
      disableStream rx_regs
      flags_rx <- dma_stream_get_isrflags rxstream
      -- Get ndtr, reset to full size:
      ndt_rx <- getReg (dmaStreamNDTR rx_regs)

      req_items <- assign (arrayLen (rx0_buf ~> stringDataL) - 1)
      setReg (dmaStreamNDTR rx_regs) $
        setField dma_sxndtr_ndt (fromRep req_items)

      -- Get current cr so we can switch ct (current target)
      cr <- getReg (dmaStreamCR rx_regs)

      -- start stream over again by clearing flags:
      dma_stream_clear_isrflags rxstream
      -- Switch current target, re-enable stream:
      modifyReg (dmaStreamCR rx_regs) $ do
        setField dma_sxcr_ct (boolToBit (iNot (bitToBool (cr #. dma_sxcr_ct))))
        setField dma_sxcr_en (fromRep 1)

      -- XXX just for inspecting that ping pong works properly from gdb:
      ifte_ (bitToBool (cr #. dma_sxcr_ct))
            (store (rx0_buf ~> stringLengthL) 1 >> store (rx1_buf ~> stringLengthL) 0)
            (store (rx0_buf ~> stringLengthL) 0 >> store (rx1_buf ~> stringLengthL) 1)

      -- XXX placeholder just to infer type of rx0 and rx1 bufs:
      ifte_ false
        (emit e (constRef rx0_buf))
        (emit e (constRef rx1_buf))


  handler req_chan "req_chan" $ callback $ \req -> do
    refCopy req_buf req
    -- XXX should do something here to assert backpressure
    -- transmit scheme is being followed, and we're not interrupting
    -- an ongoing stream request.

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
    len <- deref (req_buf ~> stringLengthL)
    req_items <- assign (safe_items len)
    modifyReg (dmaStreamNDTR tx_regs) $
      setField dma_sxndtr_ndt (fromRep req_items)

    -- Set FIFO control register:
    modifyReg (dmaStreamFCR tx_regs) $ do
      setBit   dma_sxfcr_dmdis
      setField dma_sxfcr_fth (fromRep 3)

    -- Set control register:
    modifyReg (dmaStreamCR tx_regs) $ do
      setField dma_sxcr_chsel  (fromRep (fromIntegral tx_chan))
      setField dma_sxcr_mburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_pburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_dbm    (fromRep 0) -- Single Buffering
      setField dma_sxcr_pl     (fromRep 1) -- Priority 1 (second higest after 0)
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
    -- Enable DMA Transmit in UART:
    modifyReg (uartRegCR3 uart) $ do
      setField uart_cr3_dmat (fromRep 1)

    -- Clear TC in UART, per STM32 reference RM0090 section 30.3.13
    modifyReg (uartRegSR uart) $ do
      setField uart_sr_tc (fromRep 0)

    -- Set control register:
    modifyReg (dmaStreamCR tx_regs) $ do
      setField dma_sxcr_en  (fromRep 1) -- Enable Stream

  -- Debugging states:
  tx_complete     <- state (named "tx_complete")
  tx_transfer_err <- state (named "tx_transfer_err")
  tx_direct_err   <- state (named "tx_direct_err")

  handler (dma_stream_signal txstream) "tx_stream_interrupt" $ do
    e <- emitter resp_chan 1
    callback $ const $ do
      -- Disable transmit stream:
      disableStream tx_regs
      -- Disable stream interrupts:
      modifyReg (dmaStreamCR tx_regs) $ do
        setField dma_sxcr_tcie  (fromRep 0)
        setField dma_sxcr_teie  (fromRep 0)
        setField dma_sxcr_dmeie (fromRep 0)

      -- Check stream flags:
      flags <- dma_stream_get_isrflags txstream

      let incr r = r %= (+ (1 :: Uint32))
      when (bitToBool (flags #. dma_isrflag_TCIF)) $ do
        incr tx_complete
      when (bitToBool (flags #. dma_isrflag_TEIF)) $ do
        incr tx_transfer_err
      when (bitToBool (flags #. dma_isrflag_DMEIF)) $ do
        incr tx_direct_err

      -- Clear stream flags:
      dma_stream_clear_isrflags txstream

      -- Notify request is complete. Indicates true when successful:
      emitV e (bitToBool (flags #. dma_isrflag_TCIF))

      -- Re-enable interrupt:
      dma_stream_enable_int txstream

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


