module Ivory.BSP.STM32F427.RNG
  ( rng
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32F427.Interrupt
import Ivory.BSP.STM32F427.MemoryMap
import Ivory.BSP.STM32F427.RCC

rng :: RNG
rng = mkRNG rng_periph_base
          (rccEnable rcc_ahb2en_rng)
          (rccDisable rcc_ahb2en_rng)
          HASH_RNG

rccEnable :: BitDataField RCC_AHB2ENR Bit -> Ivory eff ()
rccEnable field = modifyReg regRCC_AHB2ENR $ setBit field

rccDisable :: BitDataField RCC_AHB2ENR Bit -> Ivory eff ()
rccDisable field = modifyReg regRCC_AHB2ENR $ clearBit field
