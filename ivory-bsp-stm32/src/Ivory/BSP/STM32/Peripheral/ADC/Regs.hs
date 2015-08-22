{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- ADC peripheral registers for the STM32F4.
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.ADC.Regs where

import Ivory.BSP.STM32.Peripheral.ADC.RegTypes
import Ivory.Language

[ivory|
  bitdata ADC_SR :: Bits 32 = adc_sr
  { _ :: Bits 26
  , adc_sr_ovr :: Bit
  , adc_sr_strt :: Bit
  , adc_sr_jstrt :: Bit
  , adc_sr_jeoc :: Bit
  , adc_sr_eoc :: Bit
  , adc_sr_awd :: Bit
  }

  bitdata ADC_CR1 :: Bits 32 = adc_cr1
  { _ :: Bits 5
  , adc_cr1_ovrie :: Bit
  , adc_cr1_res :: ADCResolution
  , adc_cr1_awden :: Bit
  , adc_cr1_jawden :: Bit
  , _ :: Bits 6
  , adc_cr1_discnum :: Bits 3
  , adc_cr1_jdiscen :: Bit
  , adc_cr1_discen :: Bit
  , adc_cr1_jauto :: Bit
  , adc_cr1_awdsgl :: Bit
  , adc_cr1_scan :: Bit
  , adc_cr1_jeocie :: Bit
  , adc_cr1_awdie :: Bit
  , adc_cr1_eocie :: Bit
  , adc_cr1_awdch :: Bits 5
  }

  bitdata ADC_CR2 :: Bits 32 = adc_cr2
  { _ :: Bit
  , adc_cr2_swstart :: Bit
  , adc_cr2_exten :: Bits 2
  , adc_cr2_extsel :: Bits 4
  , _ :: Bit
  , adc_cr2_jswstart :: Bit
  , adc_cr2_jexten :: Bits 2
  , adc_cr2_jextsel :: Bits 4
  , _ :: Bits 4
  , adc_cr2_align :: Bit
  , adc_cr2_eocs :: Bit
  , adc_cr2_dds :: Bit
  , adc_cr2_dma :: Bit
  , _ :: Bits 6
  , adc_cr2_cont :: Bit
  , adc_cr2_adon :: Bit
  }

  bitdata ADC_SQR1 :: Bits 32 = adc_sqr1
  { _ :: Bits 8
  , adc_sqr1_l :: Bits 4
  , adc_sqr1_sq16 :: Bits 5
  , adc_sqr1_sq15 :: Bits 5
  , adc_sqr1_sq14 :: Bits 5
  , adc_sqr1_sq13 :: Bits 5
  }

  bitdata ADC_SQR2 :: Bits 32 = adc_sqr2
  { _ :: Bits 2
  , adc_sqr2_sq12 :: Bits 5
  , adc_sqr2_sq11 :: Bits 5
  , adc_sqr2_sq10 :: Bits 5
  , adc_sqr2_sq9 :: Bits 5
  , adc_sqr2_sq8 :: Bits 5
  , adc_sqr2_sq7 :: Bits 5
  }

  bitdata ADC_SQR3 :: Bits 32 = adc_sqr3
  { _ :: Bits 2
  , adc_sqr3_sq6 :: Bits 5
  , adc_sqr3_sq5 :: Bits 5
  , adc_sqr3_sq4 :: Bits 5
  , adc_sqr3_sq3 :: Bits 5
  , adc_sqr3_sq2 :: Bits 5
  , adc_sqr3_sq1 :: Bits 5
  }

  bitdata ADC_DR :: Bits 32 = adc_dr
  { _ :: Bits 16
  , adc_dr_data :: Bits 16
  }
|]
