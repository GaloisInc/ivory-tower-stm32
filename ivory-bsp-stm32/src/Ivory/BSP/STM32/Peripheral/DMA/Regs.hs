{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--
-- Regs.hs --- DMA Peripheral Registers
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.DMA.Regs where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.DMA.Types

[ivory|
  -- | DMA low interrupt status register.
  bitdata DMA_LISR :: Bits 32 = dma_lisr
    { _                :: Bits 4
    , dma_lisr_stream3 :: DMA_ISRFlags
    , dma_lisr_stream2 :: DMA_ISRFlags
    , _                :: Bits 4
    , dma_lisr_stream1 :: DMA_ISRFlags
    , dma_lisr_stream0 :: DMA_ISRFlags
    }

  -- | DMA high interrupt status register.
  bitdata DMA_HISR :: Bits 32 = dma_hisr
    { _                :: Bits 4
    , dma_hisr_stream7 :: DMA_ISRFlags
    , dma_hisr_stream6 :: DMA_ISRFlags
    , _                :: Bits 4
    , dma_hisr_stream5 :: DMA_ISRFlags
    , dma_hisr_stream4 :: DMA_ISRFlags
    }

  -- | DMA low interrupt flag clear register.
  bitdata DMA_LIFCR :: Bits 32 = dma_lifcr
    { _                 :: Bits 4
    , dma_lifcr_stream3 :: DMA_ClearISRFlags
    , dma_lifcr_stream2 :: DMA_ClearISRFlags
    , _                 :: Bits 4
    , dma_lifcr_stream1 :: DMA_ClearISRFlags
    , dma_lifcr_stream0 :: DMA_ClearISRFlags
    }

  -- | DMA high interrupt flag clear register.
  bitdata DMA_HIFCR :: Bits 32 = dma_hifcr
    { _                 :: Bits 4
    , dma_hifcr_stream7 :: DMA_ClearISRFlags
    , dma_hifcr_stream6 :: DMA_ClearISRFlags
    , _                 :: Bits 4
    , dma_hifcr_stream5 :: DMA_ClearISRFlags
    , dma_hifcr_stream4 :: DMA_ClearISRFlags
    }

  -- | DMA stream x configuration register.
  bitdata DMA_SxCR :: Bits 32 = dma_sxcr
    { _               :: Bits 4
    , dma_sxcr_chsel  :: Bits 3
    , dma_sxcr_mburst :: Bits 2
    , dma_sxcr_pburst :: Bits 2
    , _               :: Bit
    , dma_sxcr_ct     :: Bit
    , dma_sxcr_dbm    :: Bit
    , dma_sxcr_pl     :: Bits 2
    , dma_sxcr_pincos :: Bit
    , dma_sxcr_msize  :: Bits 2
    , dma_sxcr_psize  :: Bits 2
    , dma_sxcr_minc   :: Bit
    , dma_sxcr_pinc   :: Bit
    , dma_sxcr_circ   :: Bit
    , dma_sxcr_dir    :: Bits 2
    , dma_sxcr_pfctrl :: Bit
    , dma_sxcr_tcie   :: Bit
    , dma_sxcr_htie   :: Bit
    , dma_sxcr_teie   :: Bit
    , dma_sxcr_dmeie  :: Bit
    , dma_sxcr_en     :: Bit
    }

  -- | DMA stream x number of data register.
  bitdata DMA_SxNDTR :: Bits 32 = dma_sxndtr
    { _              :: Bits 16
    , dma_sxndtr_ndt :: Bits 16
    }

  -- | DMA stream x peripheral address register.
  bitdata DMA_SxPAR :: Bits 32 = dma_sxpar
    { dma_sxpar_par :: Bits 32
    }

  -- | DMA stream x memory 0 address register.
  bitdata DMA_SxM0AR :: Bits 32 = dma_sxm0ar
    { dma_sxm0ar_m0a :: Bits 32
    }

  -- | DMA stream x memory 1 address register.
  bitdata DMA_SxM1AR :: Bits 32 = dma_sxm1ar
    { dma_sxm1ar_m1a :: Bits 32
    }

  -- | DMA stream x FIFO control register.
  bitdata DMA_SxFCR :: Bits 32 = dma_sxfcr
    { _               :: Bits 24
    , dma_sxfcr_feie  :: Bit
    , _               :: Bit
    , dma_sxfcr_fs    :: Bits 3
    , dma_sxfcr_dmdis :: Bit
    , dma_sxfcr_fth   :: Bits 2
    }
|]

