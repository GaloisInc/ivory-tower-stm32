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

import Ivory.BSP.STM32F427.RCC
import Ivory.BSP.STM32F427.MemoryMap

ahb1Enable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Enable bit = modifyReg regRCC_AHB1ENR (setBit bit)

ahb1Disable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Disable bit = modifyReg regRCC_AHB1ENR (clearBit bit)

dma1 :: DMA
dma1 = mkDMA dma1_periph_base
             (ahb1Enable  rcc_ahb1en_dma1)
             (ahb1Disable rcc_ahb1en_dma1)
             "dma1"

dma2 :: DMA
dma2 = mkDMA dma2_periph_base
             (ahb1Enable  rcc_ahb1en_dma2)
             (ahb1Disable rcc_ahb1en_dma2)
             "dma2"
