{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Types.hs --- DMA Register Types
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.DMA.Types where

import Ivory.Language

[ivory|
  -- | Per-stream ISR flags in DMA_LISR and DMA_HISR.
  bitdata DMA_ISRFlags :: Bits 6 = dma_isrflags
    { dma_isrflag_TCIF  :: Bit -- Transfer Complete Event occured
    , dma_isrflag_HTIF  :: Bit -- Half Transfer Event occured
    , dma_isrflag_TEIF  :: Bit -- Transfer Error occured
    , dma_isrflag_DMEIF :: Bit -- Direct Mode Error occured
    , _                 :: Bit
    , dma_isrflag_FEIF  :: Bit -- FIFO Error occured
    }

  -- | Per-stream flag clear bits in DMA_LIFCR and DMA_HIFCR.
  bitdata DMA_ClearISRFlags :: Bits 6 = dma_clearisrflags
    { dma_clearisrflag_CTCIF  :: Bit -- clear Transfer Complete Event
    , dma_clearisrflag_CHTIF  :: Bit -- clear Half Transfer Event
    , dma_clearisrflag_CTEIF  :: Bit -- clear Transfer Error
    , dma_clearisrflag_CDMEIF :: Bit -- clear Direct Mode Error
    , _                       :: Bit
    , dma_clearisrflag_CFEIF  :: Bit -- clear FIFO Error
    }
|]
