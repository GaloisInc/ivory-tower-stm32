--
-- DMA.hs --- STM32F427 DMA driver.
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F427.DMA where

import Ivory.BSP.STM32.Peripheral.DMA

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32F427.RCC
import Ivory.BSP.STM32F427.MemoryMap
import Ivory.BSP.STM32F427.Interrupt

ahb1Enable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Enable bit = modifyReg regRCC_AHB1ENR (setBit bit)

ahb1Disable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Disable bit = modifyReg regRCC_AHB1ENR (clearBit bit)

dma1 :: DMA
dma1 = mkDMA dma1_periph_base
             (ahb1Enable  rcc_ahb1en_dma1)
             (ahb1Disable rcc_ahb1en_dma1)
             ints
             "dma1"
  where
  ints = DMAInterrupt
    { dmaInterruptStream0 = HasSTM32Interrupt DMA1_Stream0
    , dmaInterruptStream1 = HasSTM32Interrupt DMA1_Stream1
    , dmaInterruptStream2 = HasSTM32Interrupt DMA1_Stream2
    , dmaInterruptStream3 = HasSTM32Interrupt DMA1_Stream3
    , dmaInterruptStream4 = HasSTM32Interrupt DMA1_Stream4
    , dmaInterruptStream5 = HasSTM32Interrupt DMA1_Stream5
    , dmaInterruptStream6 = HasSTM32Interrupt DMA1_Stream6
    , dmaInterruptStream7 = HasSTM32Interrupt DMA1_Stream7
    }


dma2 :: DMA
dma2 = mkDMA dma2_periph_base
             (ahb1Enable  rcc_ahb1en_dma2)
             (ahb1Disable rcc_ahb1en_dma2)
             ints
             "dma2"
  where
  ints = DMAInterrupt
    { dmaInterruptStream0 = HasSTM32Interrupt DMA2_Stream0
    , dmaInterruptStream1 = HasSTM32Interrupt DMA2_Stream1
    , dmaInterruptStream2 = HasSTM32Interrupt DMA2_Stream2
    , dmaInterruptStream3 = HasSTM32Interrupt DMA2_Stream3
    , dmaInterruptStream4 = HasSTM32Interrupt DMA2_Stream4
    , dmaInterruptStream5 = HasSTM32Interrupt DMA2_Stream5
    , dmaInterruptStream6 = HasSTM32Interrupt DMA2_Stream6
    , dmaInterruptStream7 = HasSTM32Interrupt DMA2_Stream7
    }
