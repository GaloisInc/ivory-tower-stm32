
module Ivory.BSP.STM32F427.SPI
  ( spi1
  , spi2
  , spi3
  , spi4
  , spi5
  , spi6
  ) where


import Ivory.BSP.STM32.Peripheral.SPI

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F427.RCC
import Ivory.BSP.STM32F427.MemoryMap
import qualified Ivory.BSP.STM32F427.Interrupt as F427

spi1, spi2, spi3, spi4, spi5, spi6 :: SPIPeriph
spi1 = mkSPIPeriph spi1_periph_base rccenable rccdisable
          F427.SPI1 PClk2 "spi1"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit rcc_apb2en_spi1
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_spi1

spi2 = mkSPIPeriph spi2_periph_base rccenable rccdisable
         F427.SPI2 PClk1 "spi2"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit rcc_apb1en_spi2
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_spi2

spi3 = mkSPIPeriph spi3_periph_base rccenable rccdisable
          F427.SPI3 PClk1 "spi3"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit rcc_apb1en_spi3
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_spi3

spi4 = mkSPIPeriph spi4_periph_base rccenable rccdisable
         F427.SPI4 PClk2 "spi4"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit rcc_apb2en_spi4
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_spi4

spi5 = mkSPIPeriph spi5_periph_base rccenable rccdisable
         F427.SPI5 PClk2 "spi5"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit rcc_apb2en_spi5
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_spi5

spi6 = mkSPIPeriph spi6_periph_base rccenable rccdisable
         F427.SPI6 PClk2 "spi6"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit rcc_apb2en_spi6
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_spi6
