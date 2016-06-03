{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32.Driver.UART.DMA.Synchronous
  ( syncDMAUARTTower
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

syncDMAUARTTower :: forall tx rx e
                  . (IvoryString tx, IvoryString rx)
                 => (e -> ClockConfig)
                 -> DMAUART
                 -> UARTPins
                 -> Integer
                 -> Tower e (BackpressureTransmit tx rx, ChanOutput ('Stored ITime))
syncDMAUARTTower tocc dmauart pins baud = do
  req_chan  <- channel
  rx_chan   <- channel

  dmauart_initialized <- channel
  driver_initialized <- channel

  p <- period ms_per_frame

  mapM_ towerArtifact dmaArtifacts

  txstream <- dmaTowerStream dma (dmaUARTTxStream dmauart) (dmaUARTTxChannel dmauart)
  rxstream <- dmaTowerStream dma (dmaUARTRxStream dmauart) (dmaUARTRxChannel dmauart)

  monitor (uartName uart ++ "_dma_driver") $ do
    dmaUARTHardwareMonitor tocc dmauart pins baud
      (fst dmauart_initialized)

    syncDMAMonitor uart txstream rxstream p
      (snd req_chan) (fst rx_chan)
      (snd dmauart_initialized)
      (fst driver_initialized)
      ms_per_frame

  return (BackpressureTransmit (fst req_chan) (snd rx_chan), snd driver_initialized)

  where

  uart = dmaUARTPeriph dmauart
  dma = dmaUARTDMAPeriph dmauart

  ms_per_frame = Milliseconds $
    max 1 ((10 {- bits per byte -}
          * frame_len
          * 1000 {- ms per sec -})
      `div` baud)

  frame_len = arrayLen ((undefined :: Ref s rx) ~> stringDataL)


dmaUARTHardwareMonitor :: (e -> ClockConfig)
                       -> DMAUART
                       -> UARTPins
                       -> Integer
                       -> ChanInput ('Stored ITime)
                       -> Monitor e ()
dmaUARTHardwareMonitor tocc dmauart pins baud init_cb = do
  clockConfig <- fmap tocc getEnv
  handler systemInit "dmauart_hw_init" $ do
    e <- emitter init_cb 1
    callback $ \t -> do
      dmaRCCEnable dma
      uartInit uart pins clockConfig (fromIntegral baud) False
      emit e t
  where
  uart = dmaUARTPeriph    dmauart
  dma  = dmaUARTDMAPeriph dmauart

syncDMAMonitor :: (IvoryString tx, IvoryString rx)
               => UART
               -> DMATowerStream
               -> DMATowerStream
               -> ChanOutput ('Stored ITime)
               -> ChanOutput tx
               -> ChanInput  rx
               -> ChanOutput ('Stored ITime)
               -> ChanInput  ('Stored ITime)
               -> Milliseconds
               -> Monitor e ()
syncDMAMonitor uart txstream rxstream flush_chan req_chan rx_chan init_chan init_cb rx_period = do

  monitorModuleDef $ do
    hw_moduledef
    incl ref_to_uint32_proc

  req_buf   <- state (named "req_buf")
  rx_buf    <- state (named "rx_buf")
  tx_active <- state (named "tx_active")
  rx_active <- state (named "rx_active")
  rx_deadline <- state (named "rx_deadline")

  let rx_req_items = arrayLen (rx_buf ~> stringDataL) - 1

  handler init_chan "dmauart_tx_init" $ do
    e <- emitter init_cb 1
    callback $ \t -> do
      -- Enable TX interrupt:
      dma_stream_enable_int txstream
      store tx_active false
      store rx_active false
      -- Message init callbacks
      emit e t

  handler req_chan "req_chan" $ callback $ \req -> do
    -- Assert that the backpressure protocol is being followed.
    tx_a <- deref tx_active
    rx_a <- deref rx_active
    assert (iNot tx_a .&& iNot rx_a)
    store tx_active true
    store rx_active true

    --IMPORTANT: flush_chan has period of rx_period we need to be sure we've
    -- waited At Least that much time. Because requests are on a different
    -- clock, the worst case scenario means we have to wait two ticks before
    -- finishing the rx.
    now <- getTime
    store rx_deadline (now + (2 * (toITime rx_period)))

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
          (n <=? arrayLen (req_buf ~> stringDataL) .&& n >=? 0 .&& n <? 65535)
          ? (castWith 0 n, 0)
    len <- deref (req_buf ~> stringLengthL)
    tx_req_items <- assign (safe_items len)
    modifyReg (dmaStreamNDTR tx_regs) $
      setField dma_sxndtr_ndt (fromRep tx_req_items)

    -- Set FIFO control register:
    modifyReg (dmaStreamFCR tx_regs) $ do
      setBit   dma_sxfcr_dmdis
      setField dma_sxfcr_fth (fromRep 3)

    -- Set control register:
    modifyReg (dmaStreamCR tx_regs) $ do
      setField dma_sxcr_chsel  (fromRep (fromIntegral (dmaChannelToInt (dma_stream_channel txstream))))
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

    -------------------------------------------------------------------------

    -- Set peripheral address:
    setReg (dmaStreamPAR rx_regs) $
      setField dma_sxpar_par (fromRep (bdr_reg_addr (uartRegDR uart)))

    -- Set memory address:
    rxbuf_start_addr <- call ref_to_uint32_proc ((rx_buf ~> stringDataL) ! 0)
    setReg (dmaStreamM0AR  rx_regs) $
      setField dma_sxm0ar_m0a (fromRep rxbuf_start_addr)

    -- Set number of data items to be transfered:
    setReg (dmaStreamNDTR rx_regs) $
      setField dma_sxndtr_ndt (fromRep rx_req_items)

    -- Set FIFO control register:
    modifyReg (dmaStreamFCR rx_regs) $ do
      setBit   dma_sxfcr_dmdis
      setField dma_sxfcr_fth (fromRep 3)

    -- Set control register:
    modifyReg (dmaStreamCR rx_regs) $ do
      setField dma_sxcr_chsel  (fromRep (fromIntegral (dmaChannelToInt (dma_stream_channel rxstream))))
      setField dma_sxcr_mburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_pburst (fromRep 0) -- Single (no burst)
      setField dma_sxcr_ct     (fromRep 0) -- Current Target buf 0
      setField dma_sxcr_dbm    (fromRep 0) -- No Double Buffering
      setField dma_sxcr_pl     (fromRep 0) -- Priority 0 (highest)
      setField dma_sxcr_msize  (fromRep 0) -- Byte memory size
      setField dma_sxcr_psize  (fromRep 0) -- Byte peripheral size
      setField dma_sxcr_minc   (fromRep 1) -- Increment according to msize
      setField dma_sxcr_pinc   (fromRep 0) -- Fixed (no increment)
      setField dma_sxcr_circ   (fromRep 0) -- No circular mode
      setField dma_sxcr_dir    (fromRep 0) -- Peripheral to Memory
      setField dma_sxcr_pfctrl (fromRep 0) -- DMA is flow controller
      setField dma_sxcr_tcie   (fromRep 0) -- Enable transfer complete interrupt
      setField dma_sxcr_htie   (fromRep 0) -- Disable half transfer interrupt
      setField dma_sxcr_teie   (fromRep 1) -- Enable transfer error interrupt
      setField dma_sxcr_dmeie  (fromRep 1) -- Enable direct mode error interrupt

    -- Clear RX stream flags:
    dma_stream_clear_isrflags rxstream

    -- Enable DMA Recieve in UART:
    modifyReg (uartRegCR3 uart) $ do
      setField uart_cr3_dmar (fromRep 1)

    -- Enable RX via control register
    modifyReg (dmaStreamCR rx_regs) $ do
      setField dma_sxcr_en  (fromRep 1) -- Enable Stream

    -- Enable transmit via control register:
    modifyReg (dmaStreamCR tx_regs) $ do
      setField dma_sxcr_en  (fromRep 1) -- Enable Stream

  -- Debugging states:
  tx_complete     <- state (named "tx_complete")
  tx_transfer_err <- state (named "tx_transfer_err")
  tx_direct_err   <- state (named "tx_direct_err")

  handler (dma_stream_signal txstream) "tx_stream_interrupt" $ do
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

      -- Transmit is no longer active:
      store tx_active false

      -- Re-enable interrupt:
      dma_stream_enable_int txstream

  -- XXX FIX THIS: SHOULD BE THE IDLE INTERRUPT ON THE RX LINE
  handler flush_chan "rx_flush" $ do
    e <- emitter rx_chan 1
    callbackV $ \now -> do
      rx_a <- deref rx_active
      tx_a <- deref tx_active
      rx_dl <- deref rx_deadline
      when (rx_a .&& iNot tx_a .&& now >=? rx_dl) $ do

        -- TODO: first check uart SR for errors:
        --    ORE (overrun), NE (noise), FE (framing)
        --    ensure IDLE is set
        -- read uart DR to clear status

        -- Disable receive stream:
        disableStream rx_regs
        -- Get ndtr, reset to full size:
        ndtr <- getReg (dmaStreamNDTR rx_regs)

        -- start stream over again by clearing flags:
        dma_stream_clear_isrflags rxstream

        -- Finish rx_buf and send:
        ndt <- assign (toRep (ndtr #. dma_sxndtr_ndt))
        store (rx_buf ~> stringLengthL) (safeCast (rx_req_items - ndt))
        emit e (constRef rx_buf)

        store rx_active false

  where
  tx_regs  = dma_stream_regs  txstream
  rx_regs  = dma_stream_regs  rxstream

  named n = uartName uart ++ "_dma_" ++ n

bdr_reg_addr :: BitDataReg a -> Uint32
bdr_reg_addr = fromInteger . unReg . bdr_reg
  where unReg (Reg a) = a

ref_to_uint32_proc :: Def('[Ref s ('Stored Uint8)] ':-> Uint32)
ref_to_uint32_proc = importProc "ref_to_uint32" dmaRefToUint32Header


