module Ivory.BSP.STM32F427.ADC
  ( adc1
  , adc2
  , adc3
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32F427.Interrupt
import Ivory.BSP.STM32F427.MemoryMap
import Ivory.BSP.STM32F427.RCC

adc1 :: ADCPeriph
adc1 = mkADCPeriph adc1_periph_base
        (rccEnable rcc_apb2en_adc1)
        (rccDisable rcc_apb2en_adc1)
        ADC
        "adc1"

adc2 :: ADCPeriph
adc2 = mkADCPeriph adc2_periph_base
        (rccEnable rcc_apb2en_adc2)
        (rccDisable rcc_apb2en_adc2)
        ADC
        "adc2"

adc3 :: ADCPeriph
adc3 = mkADCPeriph adc3_periph_base
        (rccEnable rcc_apb2en_adc3)
        (rccDisable rcc_apb2en_adc3)
        ADC
        "adc3"

rccEnable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccEnable field = modifyReg regRCC_APB2ENR $ setBit field

rccDisable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccDisable field = modifyReg regRCC_APB2ENR $ clearBit field
