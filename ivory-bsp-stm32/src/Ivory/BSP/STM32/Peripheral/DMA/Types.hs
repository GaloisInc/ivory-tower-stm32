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
    { dma_isrflag_TCIF  :: Bit
    , dma_isrflag_HTIF  :: Bit
    , dma_isrflag_TEIF  :: Bit
    , dma_isrflag_DMEIF :: Bit
    , _                 :: Bit
    , dma_isrflag_FEIF  :: Bit
    }

  -- | Per-stream flag clear bits in DMA_LIFCR and DMA_HIFCR.
  bitdata DMA_ClearISRFlags :: Bits 6 = dma_clearisrflags
    { dma_clearisrflag_CTCIF  :: Bit
    , dma_clearisrflag_CHTIF  :: Bit
    , dma_clearisrflag_CTEIF  :: Bit
    , dma_clearisrflag_CDMEIF :: Bit
    , _                       :: Bit
    , dma_clearisrflag_CFEIF  :: Bit
    }
|]
