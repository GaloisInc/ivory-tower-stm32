{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Driver.DMA
  ( DMATowerStream(..)
  , dmaTowerStream
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.DMA


data DMATowerStream =
  DMATowerStream
    { dma_stream_signal         :: ChanOutput ('Stored ITime)
    , dma_stream_channel        :: DMAChannel
    , dma_stream_regs           :: DMAStreamRegs
    , dma_stream_enable_int     :: forall eff . Ivory eff ()
    , dma_stream_get_isrflags   :: forall eff . Ivory eff DMA_ISRFlags
    , dma_stream_clear_isrflags :: forall eff . Ivory eff ()
    }

dmaTowerStream :: DMA -> DMAStream -> DMAChannel ->  Tower e DMATowerStream
dmaTowerStream periph stream chan = do
  sig <- signalUnsafe (Interrupt int)
                    (Microseconds 10)
                    (do interrupt_disable int
                        modifyReg (dmaStreamCR stream_regs) $ do
                          setField dma_sxcr_tcie  (fromRep 0)
                          setField dma_sxcr_teie  (fromRep 0)
                          setField dma_sxcr_dmeie (fromRep 0)
                    )

  return (mkStream sig)
  where
  int = streamInterrupt periph stream
  stream_regs = getStreamRegs periph stream
  mkStream sig = DMATowerStream
    { dma_stream_signal         = sig
    , dma_stream_channel        = chan
    , dma_stream_regs           = stream_regs
    , dma_stream_enable_int     = interrupt_enable int
    , dma_stream_get_isrflags   = getISRFlags   periph stream
    , dma_stream_clear_isrflags = clearISRFlags periph stream
    }


