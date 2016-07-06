{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
--
-- Types.hs --- DMA Register Types
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.DMA.Peripheral where

import Text.Printf

import Ivory.Language
import Ivory.Stdlib (unless)
import Ivory.HW
import Ivory.HW.BitData
import Ivory.HW.Reg

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.DMA.Types
import Ivory.BSP.STM32.Peripheral.DMA.Regs

-- | DMA streams and channels are integers, type alias for clarity.
newtype DMAChannel = DMAChannel Int
newtype DMAStream  = DMAStream  Int

dmaStreamToInt :: DMAStream -> Int
dmaStreamToInt (DMAStream n) = n

dmaChannelToInt :: DMAChannel -> Int
dmaChannelToInt (DMAChannel n) = n

showReg :: BitDataReg d -> String
showReg r = printf "%-20s  %08X\n" name addr
  where
    name = case bdr_name r of
             Just n  -> n
             Nothing -> "(none)"
    addr = case bdr_reg r of Reg a -> a

-- | Per-DMA-stream registers for a DMA controller.
--
data DMAStreamRegs = DMAStreamRegs
  { dmaStreamCR   :: BitDataReg DMA_SxCR
  , dmaStreamNDTR :: BitDataReg DMA_SxNDTR
  , dmaStreamPAR  :: BitDataReg DMA_SxPAR
  , dmaStreamM0AR :: BitDataReg DMA_SxM0AR
  , dmaStreamM1AR :: BitDataReg DMA_SxM1AR
  , dmaStreamFCR  :: BitDataReg DMA_SxFCR
  }

showDMAStreamRegs :: DMAStreamRegs -> String
showDMAStreamRegs regs = concat $
  [ showReg (dmaStreamCR regs)
  , showReg (dmaStreamNDTR regs)
  , showReg (dmaStreamPAR regs)
  , showReg (dmaStreamM0AR regs)
  , showReg (dmaStreamM1AR regs)
  , showReg (dmaStreamFCR regs)
  ]

-- | A DMA controller peripheral.
data DMA = DMA
  { dmaRegLISR    :: BitDataReg DMA_LISR
  , dmaRegHISR    :: BitDataReg DMA_HISR
  , dmaRegLIFCR   :: BitDataReg DMA_LIFCR
  , dmaRegHIFCR   :: BitDataReg DMA_HIFCR
  , dmaStreamRegs :: [DMAStreamRegs]  -- streams 0-7
  , dmaRCCEnable  :: (forall eff. Ivory eff ())
  , dmaRCCDisable :: (forall eff. Ivory eff ())
  , dmaInterrupt  :: DMAInterrupt
  , dmaName       :: String
  }


data DMAInterrupt = DMAInterrupt
  { dmaInterruptStream0 :: HasSTM32Interrupt
  , dmaInterruptStream1 :: HasSTM32Interrupt
  , dmaInterruptStream2 :: HasSTM32Interrupt
  , dmaInterruptStream3 :: HasSTM32Interrupt
  , dmaInterruptStream4 :: HasSTM32Interrupt
  , dmaInterruptStream5 :: HasSTM32Interrupt
  , dmaInterruptStream6 :: HasSTM32Interrupt
  , dmaInterruptStream7 :: HasSTM32Interrupt
  }

----------------------------------------------------------------------
-- Register Accessors

checkStream :: DMAStream -> DMAStream
checkStream (DMAStream n) | n >= 0 && n < 8 = DMAStream n
checkStream (DMAStream n) = error $ "Invalid DMA stream: " ++ show n

-- | Get the stream registers for stream 'n'.
getStreamRegs :: DMA -> DMAStream -> DMAStreamRegs
getStreamRegs dma n = dmaStreamRegs dma !! stream
  where DMAStream stream = checkStream n

-- | Get the peripheral address register for stream 'n'.
getStreamPAR :: DMA -> DMAStream -> BitDataReg DMA_SxPAR
getStreamPAR dma n = dmaStreamPAR (getStreamRegs dma n)

-- | Get the memory address 0 register for stream 'n'.
getStreamM0AR :: DMA -> DMAStream -> BitDataReg DMA_SxM0AR
getStreamM0AR dma n = dmaStreamM0AR (getStreamRegs dma n)

-- | Disable a DMA stream. We must read the bit in a loop to make
-- sure that it is disabled because a previous request may still
-- be in progress.
disableStream :: GetBreaks (AllowBreak eff) ~ 'Break => DMAStreamRegs -> Ivory eff ()
disableStream regs = do
  let reg_SxCR = dmaStreamCR regs
  forever $ do
    modifyReg reg_SxCR $ do
      clearBit dma_sxcr_en
    cr <- getReg reg_SxCR
    unless (bitToBool (cr #. dma_sxcr_en)) breakOut

-- | Get the ISR flags subregister for stream 'n'.
getISRFlags :: DMA -> DMAStream -> Ivory eff DMA_ISRFlags
getISRFlags dma n =
  case n of
    DMAStream 0 -> go dmaRegLISR dma_lisr_stream0
    DMAStream 1 -> go dmaRegLISR dma_lisr_stream1
    DMAStream 2 -> go dmaRegLISR dma_lisr_stream2
    DMAStream 3 -> go dmaRegLISR dma_lisr_stream3
    DMAStream 4 -> go dmaRegHISR dma_hisr_stream4
    DMAStream 5 -> go dmaRegHISR dma_hisr_stream5
    DMAStream 6 -> go dmaRegHISR dma_hisr_stream6
    DMAStream 7 -> go dmaRegHISR dma_hisr_stream7
    DMAStream s -> error $ "Invalid DMA stream: " ++ show s
  where
    go :: (BitData d, IvoryIOReg (BitDataRep d),
           BitCast (BitDataRep d) (BitDataRep DMA_ISRFlags))
       => (DMA -> BitDataReg d)
       -> (BitDataField d DMA_ISRFlags)
       -> Ivory eff DMA_ISRFlags
    go reg field = do
      x <- getReg (reg dma)
      return (x #. field)

-- | Clear the ISR flags subregister for stream 'n'.
clearISRFlags :: DMA -> DMAStream -> Ivory eff ()
clearISRFlags dma n =
  case n of
    DMAStream 0 -> go dmaRegLIFCR dma_lifcr_stream0
    DMAStream 1 -> go dmaRegLIFCR dma_lifcr_stream1
    DMAStream 2 -> go dmaRegLIFCR dma_lifcr_stream2
    DMAStream 3 -> go dmaRegLIFCR dma_lifcr_stream3
    DMAStream 4 -> go dmaRegHIFCR dma_hifcr_stream4
    DMAStream 5 -> go dmaRegHIFCR dma_hifcr_stream5
    DMAStream 6 -> go dmaRegHIFCR dma_hifcr_stream6
    DMAStream 7 -> go dmaRegHIFCR dma_hifcr_stream7
    DMAStream s -> error $ "Invalid DMA stream: " ++ show s
  where
    go :: (BitData d, IvoryIOReg (BitDataRep d),
           BitCast (BitDataRep d) (BitDataRep DMA_ISRFlags))
       => (DMA -> BitDataReg d)
       -> (BitDataField d DMA_ClearISRFlags)
       -> Ivory eff ()
    go reg field = do
      setReg (reg dma) $ do
        setBit (field #> dma_clearisrflag_CTCIF)
        setBit (field #> dma_clearisrflag_CHTIF)
        setBit (field #> dma_clearisrflag_CTEIF)
        setBit (field #> dma_clearisrflag_CDMEIF)
        setBit (field #> dma_clearisrflag_CFEIF)

-- From ST application note AN4031:
--
-- To configure DMA stream x (where x is the stream number), the following
-- procedure should be applied:
--
-- 1. If the stream is enabled, disable it by resetting the EN bit in the
--    DMA_SxCR register, then read this bit in order to confirm that there is
--    no ongoing stream operation. Writing this bit to 0 is not immediately
--    effective since it is actually written to 0 once all the current
--    transfers have finished. When the EN bit is read as 0, this means
--    that the stream is ready to be configured. It is therefore necessary
--    to wait for the EN bit to be cleared before starting any stream
--    configuration. All the stream-dedicated bits set in the status
--    register (DMA_LISR and DMA_HISR) from the previous data block DMA
--    transfer should be cleared before the stream can be re-enabled.
--
--      * Ivory macro `disableStream` resets EN bit, and delays until it is cleared
--      * Ivory macro `clearISRFlags` takes care of clearing ISR Flags
--
-- 2. Set the peripheral port register address in the DMA_SxPAR register.
--    The data will be moved from/to this address to/from the peripheral
--    port after the peripheral event.
--
--      * Ivory macro `setDMAPeripheralAddress` sets this register given
--        the perhipheral port address as a Uint32
--      * TODO: add accessor to UART peripheral datastructure giving
--        peripheral address as Uint32
--
-- 3. Set the memory address in the DMA_SxMA0R register (and in the
--    DMA_SxMA1R register in the case of a Double-buffer mode).
--    The data will be written to or read from this memory after the
--    peripheral event.
--
--      * UART Transmit stream just uses MA0R
--      * UART Receive stream will use both
--
-- 4. Configure the total number of data items to be transferred in
--    the DMA_SxNDTR register. After each peripheral event or each beat
--    of the burst, this value is decremented.
--
-- 5. Select the DMA channel (request) using CHSEL[2:0] in the DMA_SxCR
--    register.
--    XXX collapse into 9
--      * User must provide the channel for the given stream that maps to the
--        intended peripheral. Lookup table can be found in AN4031 (p 9 and 10)
--        and chip family reference manual.
--
-- 6. If the peripheral is intended to be the flow controller and if it
--    supports this feature, set the PFCTRL bit in the DMA_SxCR register.
--    XXX collapse into 9
--       * only applies to SD/MMC controller peripheral. For all others, use
--         DMA_SxNTDR register, as in step 4.
--
-- 7. Configure the stream priority using the PL[1:0] bits in the DMA_SxCR
--    register.
--    XXX collapse into 9
--       * set recieve stream priotity to 0
--       * set transmit stream priority to 1
--
-- 8. Configure the FIFO usage (enable or disable, threshold in transmission
--    and reception).
--    
--       * according to examples of UART DMA I've found,
--         in SxFCR reg: set DMDIS to 1 (disable direct mode), set FTH to 0b11
--
-- 9. Configure the data transfer direction, peripheral and memory
--    incremented/fixed mode, single or burst transactions, peripheral
--    and memory data widths, Circular mode, Double-buffer mode and
--    interrupts after half and/or full transfer, and/or errors in the
--    DMA_SxCR register.
--       * UART transmit stream:
--           * MBURST: 0b00 (single)
--           * PBURST: 0b00 (single)
--           * DBM: 0
--           * MSIZE: 0 (byte)
--           * PSIZE: 0 (byte)
--           * MINC: 1 (increment according to MSIZE)
--           * PINC: 0 (fixed)
--           * CIRC: 0 (circular mode disabled)
--           * DIR: 1 (memory to peripheral)
--           * PFCTL: 0 (dma is flow controller)
--           * TCIE: 1 (transfer complete interrupt enable)
--           * HTIE: 0 (half transfer interrupt not enabled)
--           * TEIE: 1 (transfer error interrupt enable)
--           * DMEIE: 1 (direct mode error interrupt enable)
--
--       * UART recieve stream:
--           * MBURST: 0b00 (single)
--           * PBURST: 0b00 (single)
--           * CT: 0 (current buffer target)
--           * DBM: 1 (double bufer mode enabled)
--           * MSIZE: 0 (byte)
--           * PSIZE: 0 (byte)
--           * MINC: 1 (increment according to MSIZE)
--           * PINC: 0 (fixed)
--           * CIRC: 0 (circular mode disabled)
--           * DIR: 0 (peripheral to memory)
--           * PFCTL: 0 (dma is flow controller)
--           * TCIE: 1 (transfer complete interrupt enable)
--           * HTIE: 0 (half transfer interrupt not enabled)
--           * TEIE: 1 (transfer error interrupt enable)
--           * DMEIE: 1 (direct mode error interrupt enable)
--
-- 10. Activate the stream by setting the EN bit in the DMA_SxCR register.



----------------------------------------------------------------------
-- Debugging

showDMA :: DMA -> String
showDMA dma =
  "DMA Controller '" ++ dmaName dma ++ "' Registers:\n" ++
  "\n" ++
  printf "%-20s %s\n" "Name" "Address" ++
  "--------------------------------------------------------\n" ++
  concat
    [ showReg (dmaRegLISR  dma)
    , showReg (dmaRegHISR  dma)
    , showReg (dmaRegLIFCR dma)
    , showReg (dmaRegHIFCR dma)
    ] ++
  concatMap showDMAStreamRegs (dmaStreamRegs dma)

mkDMA :: Integer
      -> (forall eff. Ivory eff ())
      -> (forall eff. Ivory eff ())
      -> DMAInterrupt
      -> String
      -> DMA
mkDMA base rccEn rccDis ints name = DMA
  { dmaRegLISR      = reg 0x00 "lisr"
  , dmaRegHISR      = reg 0x04 "hisr"
  , dmaRegLIFCR     = reg 0x08 "lifcr"
  , dmaRegHIFCR     = reg 0x0c "hifcr"
  , dmaStreamRegs   = mkDMAStreamRegs 0x10
  , dmaRCCEnable    = rccEn
  , dmaRCCDisable   = rccDis
  , dmaInterrupt    = ints
  , dmaName         = name
  }
  where
    reg :: IvoryIOReg (BitDataRep d) => Integer -> String -> BitDataReg d
    reg offset fname = mkBitDataRegNamed (base + offset)
                                         (name ++ "->" ++ fname)

    -- Calculate the offset of a stream's registers. The DMA
    -- peripheral has 6 32-bit registers per stream.
    streamRegOffset :: Integer -> Integer
    streamRegOffset sn = sn * 6 * 4

    sreg :: IvoryIOReg (BitDataRep d)
         => Integer -> Integer -> Integer -> String -> BitDataReg d
    sreg off1 off2 sn fname =
      reg (off1 + (streamRegOffset sn) + off2) ("s" ++ show sn ++ fname)

    mkDMAStreamRegs :: Integer -> [DMAStreamRegs]
    mkDMAStreamRegs offset =
      [ DMAStreamRegs
        { dmaStreamCR   = sreg offset 0x00 i "cr"
        , dmaStreamNDTR = sreg offset 0x04 i "ndtr"
        , dmaStreamPAR  = sreg offset 0x08 i "par"
        , dmaStreamM0AR = sreg offset 0x0c i "m0ar"
        , dmaStreamM1AR = sreg offset 0x10 i "m1ar"
        , dmaStreamFCR  = sreg offset 0x14 i "fcr"
        }
      | i <- [0..7]]

-- | Get DMA Interrupt corresponding to given stream
streamInterrupt :: DMA -> DMAStream -> HasSTM32Interrupt
streamInterrupt dma n =
  case n of
    DMAStream 0 -> dmaInterruptStream0 ints
    DMAStream 1 -> dmaInterruptStream1 ints
    DMAStream 2 -> dmaInterruptStream2 ints
    DMAStream 3 -> dmaInterruptStream3 ints
    DMAStream 4 -> dmaInterruptStream4 ints
    DMAStream 5 -> dmaInterruptStream5 ints
    DMAStream 6 -> dmaInterruptStream6 ints
    DMAStream 7 -> dmaInterruptStream7 ints
    DMAStream s -> error $ "Invalid DMA stream: " ++ show s
  where ints = dmaInterrupt dma
