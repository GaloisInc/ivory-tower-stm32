{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32.Driver.UART.DMA
  ( dmaUARTTower
  , dmaUARTTower'
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

dmaUARTTower :: forall tx rx e
              . (IvoryString tx, IvoryString rx)
             => (e -> ClockConfig)
             -> DMAUART
             -> UARTPins
             -> Integer
             -> Proxy rx
             -> Tower e ( BackpressureTransmit tx ('Stored IBool)
                        , ChanOutput ('Stored Uint8)
                        , Monitor e ())
dmaUARTTower tocc dmauart pins baud _ = do
  (tx, (buf_rx :: ChanOutput rx), mon) <- dmaUARTTower' tocc dmauart pins baud
  char_rx <- channel
  monitor (uartName uart ++ "_dma_rx_byte_shim") $ do
    handler buf_rx (uartName uart ++ "_buf_rx") $ do
      e <- emitter (fst char_rx) (arrayLen (somebuf ~> stringDataL))
      callback $ \buf -> do
        len <- deref (buf ~> stringLengthL)
        arrayMap $ \ix -> do
          when (fromIx ix <? len) $
            emit e ((buf ~> stringDataL) ! ix)
  return (tx, snd char_rx, mon)
  where
  uart = dmaUARTPeriph dmauart
  somebuf :: Ref s rx
  somebuf = undefined

dmaUARTTower' :: forall tx rx e
              . (IvoryString tx, IvoryString rx)
             => (e -> ClockConfig)
             -> DMAUART
             -> UARTPins
             -> Integer
             -> Tower e ( BackpressureTransmit tx ('Stored IBool)
                        , ChanOutput rx
                        , Monitor e ())
dmaUARTTower' tocc dmauart pins baud = do
  req_chan  <- channel
  resp_chan <- channel
  rx_chan   <- channel

  dmauart_initialized <- channel

  p <- period (Milliseconds ms_per_frame)

  mapM_ towerArtifact dmaArtifacts

  txstream <- dmaTowerStream dma (dmaUARTTxStream dmauart) (dmaUARTTxChannel dmauart)
  rxstream <- dmaTowerStream dma (dmaUARTRxStream dmauart) (dmaUARTRxChannel dmauart)

  let mon = do
        dmaUARTHardwareMonitor tocc dmauart pins baud
          (fst dmauart_initialized)

        dmaUARTTransmitMonitor uart txstream (snd req_chan) (fst resp_chan)
          (snd dmauart_initialized)

        dmaUARTReceiveMonitor  uart rxstream (fst rx_chan) p
          (snd dmauart_initialized)

  return (BackpressureTransmit (fst req_chan) (snd resp_chan), (snd rx_chan), mon)

  where

  uart = dmaUARTPeriph    dmauart
  dma  = dmaUARTDMAPeriph dmauart


  ms_per_frame = max 1 ((10 {- bits per byte -}
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

dmaUARTTransmitMonitor :: (IvoryString tx)
                       => UART
                       -> DMATowerStream
                       -> ChanOutput tx
                       -> ChanInput  ('Stored IBool)
                       -> ChanOutput ('Stored ITime)
                       -> Monitor e ()
dmaUARTTransmitMonitor uart txstream req_chan resp_chan init_chan = do

  monitorModuleDef $ do
    hw_moduledef
    incl ref_to_uint32_proc

  req_buf   <- state (named "req_buf")
  tx_active <- state (named "tx_active")

  handler init_chan "dmauart_tx_init" $ callback $ const $ do
    -- Enable TX interrupt:
    dma_stream_enable_int txstream
    store tx_active false


  handler req_chan "req_chan" $ callback $ \req -> do
    -- XXX should do something here to assert backpressure
    -- transmit scheme is being followed, and we're not interrupting
    -- an ongoing stream request.
    a <- deref tx_active
    assert (iNot a)
    store tx_active true

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
    len       <- deref  (req_buf ~> stringLengthL)
    req_items <- assign (safe_items len)
    modifyReg (dmaStreamNDTR tx_regs) $
      setField dma_sxndtr_ndt (fromRep req_items)

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
      store tx_active false

      -- Re-enable interrupt:
      dma_stream_enable_int txstream

  where
  tx_regs  = dma_stream_regs  txstream

  named n = uartName uart ++ "_dma_" ++ n



dmaUARTReceiveMonitor :: forall rx e
                       . (IvoryString rx)
                      => UART
                      -> DMATowerStream
                      -> ChanInput  rx
                      -> ChanOutput ('Stored ITime)
                      -> ChanOutput ('Stored ITime)
                      -> Monitor e ()
dmaUARTReceiveMonitor uart rxstream out_chan flush_chan init_chan = do
  rx0_buf <- state (named "rx0_buf")
  rx1_buf <- state (named "rx1_buf")
  isr_buf <- state (named "isr_buf")

  let req_items = arrayLen (rx0_buf ~> stringDataL) - 1

  handler init_chan "dmauart_rx_init" $ callback $ const $ do
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
    setReg (dmaStreamNDTR rx_regs) $
      setField dma_sxndtr_ndt (fromRep req_items)

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

  let complete_buffer :: Uint16
                      -> IBool
                      -> (ConstRef 'Global rx -> Ivory eff ())
                      -> Ivory eff ()
      complete_buffer len which_buf k = do
        ifte_ which_buf
          (aux rx1_buf)
          (aux rx0_buf)
        where
        aux buf = do
          store (buf ~> stringLengthL) (safeCast len)
          k (constRef buf)

  -- Debugging states:
  rx_complete     <- state (named "rx_complete")
  rx_transfer_err <- state (named "rx_transfer_err")
  rx_direct_err   <- state (named "rx_direct_err")

  handler (dma_stream_signal rxstream) "rx_stream_interrupt" $ do
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
      modifyReg (dmaStreamCR rx_regs) $ do
        setField dma_sxcr_tcie   (fromRep 1)
        setField dma_sxcr_htie   (fromRep 0)
        setField dma_sxcr_teie   (fromRep 1)
        setField dma_sxcr_dmeie  (fromRep 1)

      when (bitToBool (flags #. dma_isrflag_TCIF)) $ do
        -- Determine which buffer is now complete:
        cr <- getReg (dmaStreamCR rx_regs)
        finished_buf <- assign (iNot (bitToBool (cr #. dma_sxcr_ct)))

        -- Defer completed buffer to be sent by flush:
        complete_buffer req_items finished_buf (refCopy isr_buf)

      -- Enable RX interrupt:
      dma_stream_enable_int rxstream

  handler flush_chan "rx_flush" $ do
    e <- emitter out_chan 2
    callback $ const $ do
      -- Disable receive stream:
      disableStream rx_regs
      -- Get ndtr, reset to full size:
      ndtr <- getReg (dmaStreamNDTR rx_regs)

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

      -- Check to see if we got a buffer from the rx_stream_interupt:
      isr_buf_len <- deref (isr_buf ~> stringLengthL)
      when (isr_buf_len >? 0) $ do
        emit e (constRef isr_buf)
        store (isr_buf ~> stringLengthL) 0

      ndt <- assign (toRep (ndtr #. dma_sxndtr_ndt))
      -- When at least one item has been recieved:
      when (ndt <? req_items) $ do
        -- Determine which buffer is complete:
        finished_buf <- assign (bitToBool (cr #. dma_sxcr_ct))
        complete_buffer (req_items - ndt) finished_buf (emit e)

  where
  named n = uartName uart ++ "_dma_" ++ n

  rx_regs  = dma_stream_regs  rxstream

bdr_reg_addr :: BitDataReg a -> Uint32
bdr_reg_addr = fromInteger . unReg . bdr_reg
  where unReg (Reg a) = a

ref_to_uint32_proc :: Def('[Ref s ('Stored Uint8)] ':-> Uint32)
ref_to_uint32_proc = importProc "ref_to_uint32" dmaRefToUint32Header


